import {
	Interface,
	literal,
	opt,
	schema,
	seq,
	takeAllOrSkip,
	takeAndSkipMany,
} from "./sdk.ts";

// Define tools for org file operations
const [oaiSpec, validator] = schema({
	addTask: {
		0: "Adds a new task/subtask to the org file",
		title: "Task title (required)",
		parentId: "ID of parent task or 'root' for top-level tasks",
		type: "Type ('TASK' or 'NOTE')",
	},
	updateTask: {
		0: "Updates task properties via PATCH-like operation",
		id: "Unique task ID to update",
		priority: "Priority letter (A-H)",
		startDate: "Start date in <YYYY-MM-DD Mon HH:MM> format",
		dueDate: "Due date in <YYYY-MM-DD Tue HH:MM> format",
		scheduleDate:
			"Schedule date for recurring tasks in <YYYY-MM-DD Mon HH:MM-HH:MM> format",
	},
	removeTask: {
		0: "Removes a task and all subtasks",
		id: "Unique task ID to remove",
	},
	moveTask: {
		0: "Moves a task under a new parent",
		id: "ID of task to move",
		newParentId: "New parent task ID or 'root'",
		position:
			"Position relative to siblings ('first', 'last', index number)",
	},
	copyTask: {
		0: "Copies a task with all subtasks under new parent",
		id: "ID of task to copy",
		newParentId: "New parent task ID or 'root'",
		position:
			"Position relative to siblings ('first', 'last', index number)",
	},
});

// Enhanced System Prompt with Autonomous Task Handling
const system = `
You are an autonomous task organizer managing an org-mode file. Your primary goal is to process inputs, structure tasks, and make decisions without unnecessary user interruptions.

Key Features:
1. Tasks have IDs, priorities (A-H), deadlines, start dates, schedules, and nested subtasks
2. Automatically categorize knowledge as notes under relevant tasks
3. Maintain real-time status updates in the org file

# Task Processing Rules
- When you do not have any questions to ask from the user or have no pending operations to complete such as, when the user does not give any command, you just respond with '[READY]'. Only ask questions like 'What task would you like to add next?' when the user message contains '[PROMPT]'.
- When sufficient information is available, directly execute task operations using tools
- Only request clarification via context if critical details are missing (e.g., missing title/priority), otherwise just respond with '[READY]' after you have successfully performed the operation, or if you think you won't be able to complete the operation with the given tools.
- Prioritize tasks based on urgency (due date) and importance (priority letter A-H)

# Knowledge Organization Workflow
1. New concepts → Auto-create top-level TASKs with default priority (C)
2. Related information → Add as subtasks/NOTEs under relevant parent
3. Assign priorities: A=Urgent/Important, H=Lowest
4. Set due dates for time-sensitive tasks
5. Maintain clean org structure with proper indentation

# Tools
You may call one or more functions to assist with the user query. Function signatures:
\`\`\`json
${JSON.stringify(oaiSpec)}
\`\`\`

For each function call, return a json object with function name and arguments within code blocks:
\`\`\`json
{"name": <function-name>, "arguments": <args-json-object>}
\`\`\`
You may make multiple tool calls in a single response.
`;

const ifc = await Interface.withDefaults({
	system,
	base_url: "https://192.168.1.35",
	model: "Gemma-3",
});

const addTask = async ({
	title,
	parentId = "root",
	type = "TASK",
}: {
	title: string;
	parentId?: string;
	type?: string;
}) => {
	try {
		const content = await Deno.readTextFile("knowledge.org");
		const lines = content.split("\n");

		const taskId = crypto.randomUUID();

		let parentPos = -1;
		let indentLevel = 0;
		let endOfParentProps = -1;

		if (parentId !== "root") {
			for (let i = 0; i < lines.length; i++) {
				const headingMatch = lines[i].match(/^(\*+)\s*/);
				if (!headingMatch) continue;

				let propsStart = -1;
				for (let j = i + 1; j < lines.length; j++) {
					if (lines[j].match(/^:PROPERTIES:/)) {
						propsStart = j;
						break;
					}
				}

				if (propsStart === -1) continue;

				let foundId = false;
				for (let k = propsStart + 1; k < lines.length; k++) {
					if (lines[k].match(/^:END:/)) break;
					if (lines[k].includes(`:ID: ${parentId}`)) {
						foundId = true;
						parentPos = i;
						indentLevel = headingMatch[1].length;
						endOfParentProps = k + 1;
						break;
					}
				}

				if (foundId) {
					while (
						endOfParentProps < lines.length &&
						!lines[endOfParentProps].match(/^:END:/)
					) {
						endOfParentProps++;
					}
					if (endOfParentProps >= lines.length) {
						return `<tool_response>Invalid parent PROPERTIES block for ID ${parentId}</tool_response>\n`;
					}
					break;
				}
			}

			if (parentPos === -1) {
				return `<tool_response>Parent task with ID ${parentId} not found</tool_response>\n`;
			}
		} else {
			endOfParentProps = lines.length;
		}

		const newTaskIndent = "*".repeat(indentLevel + 1);
		const newTaskLines = [
			`${newTaskIndent} ${title}`,
			":PROPERTIES:",
			`:ID: ${taskId}`,
			type === "TASK" ? ":TYPE: TASK" : ":TYPE: NOTE",
			":END:",
		];

		let insertPos;
		if (parentId !== "root") {
			insertPos = endOfParentProps + 1;
		} else {
			insertPos = lines.length;
		}

		if (lines.length === 0 || (lines.length === 1 && lines[0] === "")) {
			insertPos = 0;
		}

		lines.splice(insertPos, 0, ...newTaskLines);

		await Deno.writeTextFile(
			"knowledge.org",
			lines.join("\n").trim() + "\n",
		);

		return `<tool_response>Created task "${title}" with ID ${taskId}</tool_response>\n`;
	} catch (err) {
		return `<tool_response>Error: ${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const updateTask = async ({
	id,
	priority,
	startDate,
	dueDate,
	scheduleDate,
}: {
	id: string;
	priority?: string;
	startDate?: string;
	dueDate?: string;
	scheduleDate?: string;
}) => {
	try {
		const content = await Deno.readTextFile("knowledge.org");
		const lines = content.split("\n");
		let taskPos = -1;

		for (let i = 0; i < lines.length; i++) {
			if (lines[i].includes(`:ID: ${id}`)) {
				taskPos = i;
				break;
			}
		}

		if (taskPos === -1) {
			return `<tool_response>Task not found</tool_response>\n`;
		}

		const propertiesStart = taskPos;
		let propertiesEnd = propertiesStart + 1;

		while (!lines[propertiesEnd].startsWith(":END:")) {
			propertiesEnd++;
		}

		if (priority) {
			lines.splice(propertiesStart, 0, `:PRIORITY: ${priority}`);
		}

		if (startDate) {
			lines.splice(propertiesStart, 0, `:STARTDATE: <${startDate}>`);
		}

		if (dueDate) {
			lines.splice(propertiesStart, 0, `:DEADLINE: <${dueDate}>`);
		}

		if (scheduleDate) {
			lines.splice(propertiesStart, 0, `:SCHEDULED: <${scheduleDate}>`);
		}

		await Deno.writeTextFile("knowledge.org", lines.join("\n"));
		return `<tool_response>Updated task ${id}</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const removeTask = async ({ id }: { id: string }) => {
	try {
		const content = await Deno.readTextFile("knowledge.org");
		const lines = content.split("\n");

		// Find the task's heading index and ID line
		const taskInfo = findTaskHeadingIndex(lines, id);
		if (!taskInfo) {
			return `<tool_response>Task ${id} not found</tool_response>\n`;
		}
		const [headingIdx] = taskInfo;

		// Determine where the task ends (exclusive)
		const endLine = findTaskEnd(lines, headingIdx);

		// Calculate the number of lines to remove
		const linesToRemove = endLine - headingIdx;

		// Remove the task and its subtree from the lines array
		lines.splice(headingIdx, linesToRemove);

		// Save the modified content back to the file
		await Deno.writeTextFile("knowledge.org", lines.join("\n"));

		return `<tool_response>Task ${id} deleted successfully</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const moveTask = async ({
	id,
	newParentId,
	position,
}: {
	id: string;
	newParentId: string;
	position: string | number;
}) => {
	try {
		const content = await Deno.readTextFile("knowledge.org");
		const lines = content.split("\n");

		// Find the task's heading index and ID line
		const taskInfo = findTaskHeadingIndex(lines, id);
		if (!taskInfo) {
			return `<tool_response>Task ${id} not found</tool_response>\n`;
		}
		const [headingIdx] = taskInfo;

		// Determine where the task ends (exclusive)
		const endLine = findTaskEnd(lines, headingIdx);

		// Extract original task block
		const originalBlock = lines.slice(headingIdx, endLine);

		// Get current indentation level of the task's heading
		const currentIndentLevel =
			(lines[headingIdx].match(/^(\*+)/)!)[1].length;

		// Validate new parent exists (if not root)
		const newParentInfo = findNewParentInfo(lines, newParentId);
		if (!newParentInfo && newParentId !== "root") {
			return `<tool_response>New parent ${newParentId} not found</tool_response>\n`;
		}

		// Calculate new indent level for the task's heading under new parent
		const newIndentLevel = newParentId === "root"
			? 1
			: (newParentInfo!.indentLevel + 1);

		// Adjust block indentation (same as copyTask)
		const adjustedBlock = originalBlock.map((line) => {
			const starMatch = line.match(/^(\*+)/);
			if (!starMatch) return line;

			const originalStars = starMatch[1].length;
			// Only process lines that are part of the task's subtree (indent >= heading indent)
			if (originalStars < currentIndentLevel) return line;

			const depth = originalStars - currentIndentLevel;
			const newStarCount = newIndentLevel + depth;

			return line.replace(starMatch[1], "*".repeat(newStarCount));
		});

		// Remove the original task block from lines
		const linesToRemove = endLine - headingIdx;
		lines.splice(headingIdx, linesToRemove);

		// Calculate insert position in the new location
		const insertPos = calculateInsertPosition(
			lines,
			newParentId,
			newParentId === "root" ? null : newParentInfo,
			position,
		);

		// Insert the adjusted block at the calculated position
		lines.splice(insertPos, 0, ...adjustedBlock);

		// Save the modified content
		await Deno.writeTextFile("knowledge.org", lines.join("\n"));

		return `<tool_response>Task ${id} moved successfully</tool_response>\n`;
	} catch (error) {
		console.error("Move task error:", error);
		return `<tool_response>Error: ${String(error)}</tool_response>\n`;
	}
};

// Copy task implementation (revised)
export async function copyTask({
	id,
	newParentId = "root",
	position = "last",
}: {
	id: string;
	newParentId?: string;
	position?: string | number;
}) {
	try {
		if (position !== "first" && position !== "last") {
			position = Number(position);
		}
		const content = await Deno.readTextFile("knowledge.org");
		const lines = content.split("\n");

		// Find original task
		const taskInfo = findTaskHeadingIndex(lines, id);
		if (!taskInfo) {
			return `<tool_response>Task ${id} not found</tool_response>\n`;
		}
		const [headingIdx, _idLine] = taskInfo;

		// Get task block
		const taskEnd = findTaskEnd(lines, headingIdx);
		const originalBlock = lines.slice(headingIdx, taskEnd);

		// Generate new ID and adjust block
		const newId = `copy-${id}-${Date.now()}`;
		const currentIndentLevel =
			(lines[headingIdx].match(/^(\*+)/)!)[1].length;
		const parentInfo = findNewParentInfo(lines, newParentId);
		if (!parentInfo && newParentId !== "root") {
			return `<tool_response>New parent ${newParentId} not found</tool_response>\n`;
		}

		// Calculate new indentation level for the heading
		const newIndentLevel = newParentId === "root"
			? 1
			: (parentInfo!.indentLevel + 1);

		// Adjust block indentation with depth preservation
		const adjustedBlock = originalBlock.map((line) => {
			const starMatch = line.match(/^(\*+)/);
			if (!starMatch) return line; // Non-headline lines

			const originalStars = starMatch[1].length;
			// Only process lines that are part of the task's subtree (indent >= heading indent)
			if (originalStars < currentIndentLevel) return line;

			// Calculate depth relative to original heading
			const depth = originalStars - currentIndentLevel;
			// Compute new indentation based on target parent and depth
			const newStarCount = newIndentLevel + depth;

			// Replace the stars with new count while preserving rest of the line
			return line.replace(starMatch[1], "*".repeat(newStarCount));
		});

		// Update ID in the new block
		const idLineInBlock = adjustedBlock.findIndex((l) =>
			l.includes(`:ID: ${id}`)
		);
		if (idLineInBlock !== -1) {
			adjustedBlock[idLineInBlock] = adjustedBlock[idLineInBlock].replace(
				`:ID: ${id}`,
				`:ID: ${newId}`,
			);
		}

		// Calculate insertion position
		const insertPos = calculateInsertPosition(
			lines,
			newParentId,
			newParentId === "root" ? null : parentInfo!,
			position,
		);

		// Insert and save
		lines.splice(insertPos, 0, ...adjustedBlock);
		await Deno.writeTextFile("knowledge.org", lines.join("\n"));
		return `<tool_response>Task copied. New ID: ${newId}</tool_response>\n`;
	} catch (err) {
		return `<tool_response>Error copying task: ${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
}

// Keep helper functions unchanged unless needed
function findTaskHeadingIndex(
	lines: string[],
	id: string,
): [number, number] | null {
	let taskIdLine = -1;
	for (let i = 0; i < lines.length; i++) {
		if (lines[i].includes(`:ID: ${id}`)) {
			taskIdLine = i;
			break;
		}
	}
	if (taskIdLine === -1) return null;

	let headingIndex = taskIdLine - 1;
	while (headingIndex >= 0 && !lines[headingIndex].match(/^(\*+)\s*/)) {
		headingIndex--;
	}
	if (headingIndex < 0) return null;

	return [headingIndex, taskIdLine];
}

function findTaskEnd(lines: string[], startLine: number): number {
	const currentIndent = (lines[startLine].match(/^(\*+)/)!)[1].length;
	let endLine = startLine + 1;
	while (endLine < lines.length) {
		const line = lines[endLine];
		const indentMatch = line.match(/^(\*+)/);
		if (indentMatch && indentMatch[1].length <= currentIndent) break;
		endLine++;
	}
	return endLine;
}

function findNewParentInfo(
	lines: string[],
	newParentId: string,
): { headingIndex: number; indentLevel: number } | null {
	if (newParentId === "root") return { headingIndex: -1, indentLevel: 0 };

	for (let i = 0; i < lines.length; i++) {
		const line = lines[i];
		if (!line.match(/^(\*+)\s*/)) continue;

		let propsStart = -1;
		let j = i + 1;
		while (j < lines.length && !lines[j].includes(":PROPERTIES:")) {
			if (/^\s*$/.test(lines[j]) || lines[j].startsWith("#+")) {
				j++;
				continue;
			}
			break;
		}

		if (j >= lines.length) continue;

		propsStart = j + 1;
		let hasId = false;
		while (
			propsStart < lines.length && !lines[propsStart].includes(":END:")
		) {
			if (lines[propsStart].includes(`:ID: ${newParentId}`)) {
				hasId = true;
				break;
			}
			propsStart++;
		}
		if (hasId) {
			const indentLevel = (line.match(/^(\*+)/)!)[1].length;
			return { headingIndex: i, indentLevel };
		}
	}
	return null;
}

function calculateInsertPosition(
	lines: string[],
	newParentId: string,
	newParentInfo: { headingIndex: number; indentLevel: number } | null,
	position: string | number,
): number {
	if (position !== "first" && position !== "last") {
		position = Number(position);
	}
	if (newParentId === "root") {
		const topLevel: number[] = [];
		for (let i = 0; i < lines.length; i++) {
			const m = lines[i].match(/^(\*+)/);
			if (m && m[1].length === 1) topLevel.push(i);
		}

		if (typeof position === "number") {
			return Math.max(0, Math.min(position, topLevel.length));
		}
		return position === "last"
			? (topLevel.length > 0 ? topLevel[topLevel.length - 1] + 1 : 0)
			: 0;
	} else {
		const { headingIndex: parentIdx, indentLevel: parentIndent } =
			newParentInfo!;

		let parentEnd = lines.length;
		let currentLine = parentIdx + 1;
		while (currentLine < lines.length) {
			const line = lines[currentLine];
			const indentMatch = line.match(/^(\*+)/);
			if (!indentMatch) {
				currentLine++;
				continue;
			}
			const currentIndentLevel = indentMatch[1].length;
			if (currentIndentLevel <= parentIndent) {
				parentEnd = currentLine;
				break;
			}
			currentLine++;
		}

		const contentStart = parentIdx + 1;
		const contentEnd = parentEnd - 1;

		let bodyStart = contentStart;
		for (let i = contentStart; i <= contentEnd; i++) {
			if (lines[i].includes(":PROPERTIES:")) {
				let propsEnd = i + 1;
				while (
					propsEnd <= contentEnd && !lines[propsEnd].includes(":END:")
				) {
					propsEnd++;
				}
				bodyStart = propsEnd + 1;
				break;
			}
		}

		const children: number[] = [];
		for (let i = bodyStart; i <= contentEnd; i++) {
			const lineIndentMatch = lines[i].match(/^(\*+)/);
			if (!lineIndentMatch) continue;
			const lineIndent = lineIndentMatch[1].length;
			if (lineIndent === parentIndent + 1) {
				children.push(i);
			}
		}

		if (typeof position === "number") {
			return Math.max(0, Math.min(position, children.length));
		}
		return position === "last"
			? (children.length > 0 ? contentEnd + 1 : bodyStart)
			: (children.length > 0 ? children[0] : bodyStart);
	}
}

// Directory-based instruction handling
const INSTRUCTION_DIR = "./instructions";

const processInstructions = async (): Promise<string> => {
	await Deno.mkdir(INSTRUCTION_DIR, { recursive: true });
	let userInput = "[PROMPT].\n\n";

	// Read all text files in instruction directory
	for await (const entry of Deno.readDir(INSTRUCTION_DIR)) {
		if (entry.isFile) {
			const filePath = `${INSTRUCTION_DIR}/${entry.name}`;
			try {
				// Read content and delete file immediately
				userInput = await Deno.readTextFile(filePath).then((c) =>
					c.trim()
				);
				await Deno.remove(filePath);
				break;
			} catch (_err) {
				_err;
			}
		}
	}

	return userInput;
};

const dmenuAsk = async ({ question }: { question: string }) => {
	try {
		const process = new Deno.Command("tofi", {
			args: ["--prompt-text", question],
			stdin: "null",
			stdout: "piped",
			stderr: "piped",
		})
			.spawn();
		const output = await process.output();
		if (output.code) {
			const error = await new Response(output.stderr).text();
			return `stderr: ${error}\n`;
		}
		const userInput = await new Response(output.stdout).text();
		return userInput;
	} catch (err) {
		return `${JSON.stringify({ error: err })}\n`;
	}
};

// Main execution
ifc.response === "" && ifc.keepAndReturn();

let [_, { context, toolCalls }] = seq(
	takeAllOrSkip("context", "```"),
	opt(literal("json")),
	opt(literal("\n")),
	takeAndSkipMany(
		"toolCalls",
		"```",
		seq(
			opt(literal("json")),
			literal("\n"),
		),
	),
)([ifc.response, {}]);
context = context ?? "";
toolCalls = toolCalls ?? [];

let toolResp = "";

for (const toolCall of toolCalls) {
	if (toolCall.includes("addTask")) {
		toolResp += await addTask(
			validator<"addTask">(toolCall).arguments,
		);
	} else if (toolCall.includes("updateTask")) {
		toolResp += await updateTask(
			validator<"updateTask">(toolCall).arguments,
		);
	} else if (toolCall.includes("removeTask")) {
		toolResp += await removeTask(
			validator<"removeTask">(toolCall).arguments,
		);
	} else if (toolCall.includes("moveTask")) {
		toolResp += await moveTask(
			validator<"moveTask">(toolCall).arguments,
		);
	} else if (toolCall.includes("copyTask")) {
		toolResp += await copyTask(
			validator<"copyTask">(toolCall).arguments,
		);
	}
}

// Always update status with current org file content
const question = context.trim();
const user = question === "[READY]"
	? await processInstructions()
	: question !== ""
	? await dmenuAsk({ question })
	: "";
const status = await Deno.readTextFile("knowledge.org").then((text) =>
	"\nThe current contents are:\n" + text
).catch(() => "");
if (status !== "") ifc.clearStatus();

const assistant = "<tool_call>" +
	toolCalls.join("</tool_call>\n<tool_call>") +
	"</tool_call>";

ifc.pushAndReturn({ context, assistant, tool: toolResp, user, status });
