import {
	Interface,
	schema,
	seq,
	takeAllOrSkip,
	takeAndSkip,
	takeAndSkipMany,
} from "./sdk.ts";

// Define tools for directory operations
const [oaiSpec, validator] = schema({
	inspect: {
		0: "Shows the current directory structure of the knowledge base folder, including every node",
	},
	mkdir: {
		0: "Creates a new concept node in the knowledge graph",
		path: "Path to create as a directory (e.g., 'concepts/math/algebra')",
	},
	lnSym: {
		0: "Creates symbolic link between related concepts",
		source: "Existing file/directory path to link from",
		target: "New symbolic link name (full path)",
	},
	mv: {
		0: "Renames or moves a concept in the graph",
		oldPath: "Current path of the concept node",
		newPath: "New path for the concept node",
	},
	rmRf: {
		0: "Removes a concept and all sub-concepts inside the directory",
		path: "Path to remove recursively",
	},
});

// System prompt with knowledge base instructions
const system = `
You are building an interactive knowledge base using directory structure.
- Use mkdir to create new concepts as directories
- The name of the folders represent the content of concepts
- Use lnSym to link related concepts (graph edges)
- After properly storing and linking related concepts using the provided tools, ask a question from the user to rinse and repeat

Knowledge Base Rules:
1. Each concept is a directory with name used for storing content
2. Relationships are symbolic links between directories
3. Maintain acyclic structure - no circular references
4. Always ask 1 question from the user before the <tool_calls></tool_calls> section

# Tools

You may call one or more functions to assist with the user query.

You are provided with function signatures within <tools></tools> XML tags:
<tools>
${JSON.stringify(oaiSpec)}
</tools>

For each function call, return a json object with function name and arguments within <tool_call></tool_call> XML tags:
<tool_call>
{"name": <function-name>, "arguments": <args-json-object>}
</tool_call>
You may make multiple tool calls in a single response.

/no_think
`;

const ifc = await Interface.withDefaults({
	system,
	base_url: "https://192.168.1.35",
	model: "Qwen3",
});

// Tool implementations
const mkdir = async ({ path }: { path: string }) => {
	try {
		const fullpath = `knowledge_base/${path}`;
		await Deno.mkdir(fullpath, { recursive: true });

		return `<tool_response>Created concept at ${fullpath}. Ask questions to expand this node.</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const lnSym = async (
	{ source, target }: { source: string; target: string },
) => {
	try {
		const fullSource = `knowledge_base/${source}`;
		const fullTarget = `knowledge_base/${target}`;

		// Ensure source exists and is a directory
		if (!(await Deno.stat(fullSource)).isDirectory) {
			return `<tool_response>${
				JSON.stringify({
					error: "Source must be an existing directory",
				})
			}</tool_response>\n`;
		}

		await Deno.symlink(fullSource, fullTarget);
		return `<tool_response>Created relationship between ${source} and ${target}.</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const mv = async (
	{ oldPath, newPath }: { oldPath: string; newPath: string },
) => {
	try {
		const source = `knowledge_base/${oldPath}`;
		const destination = `knowledge_base/${newPath}`;

		// Ensure we're not moving outside knowledge_base
		if (!source.startsWith("knowledge_base/")) {
			return `<tool_response>${
				JSON.stringify({
					error: "Paths must be within knowledge_base directory",
				})
			}</tool_response>\n`;
		}

		await Deno.rename(source, destination);
		return `<tool_response>Renamed/moved ${oldPath} to ${newPath}.</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const rmRf = async ({ path }: { path: string }) => {
	try {
		const fullpath = `knowledge_base/${path}`;

		// Ensure we're not removing outside knowledge_base
		if (!fullpath.startsWith("knowledge_base/")) {
			return `<tool_response>${
				JSON.stringify({
					error: "Paths must be within knowledge_base directory",
				})
			}</tool_response>\n`;
		}

		await Deno.remove(fullpath, { recursive: true });
		return `<tool_response>Removed concept at ${path} and all sub-concepts.</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${
			JSON.stringify({ error: err })
		}</tool_response>\n`;
	}
};

const inspect = async ({}: object) => {
	try {
		const fs = await new Deno.Command("erd", {
			args: ["-I", "--suppress-size", "src"],
			env: { NO_COLOR: "1" },
		})
			.output();
		return `<tool_response>${
			JSON.stringify({
				stdout: await new Response(fs.stdout).text(),
				stderr: await new Response(fs.stderr).text(),
			})
		}</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${JSON.stringify(err)}</tool_response>\n`;
	}
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
			return `<tool_response>stderr: ${error}</tool_response>\n`;
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
	takeAndSkip("_", "</think>"),
	takeAllOrSkip("context", "<tool_call>"),
	takeAndSkipMany(
		"toolCalls",
		"</tool_call>",
		takeAllOrSkip("_", "<tool_call>"),
	),
)([ifc.response, {}]);
context = context ?? "";
toolCalls = toolCalls ?? [];

let toolResp = "";

for (const toolCall of toolCalls) {
	if (toolCall.includes("mkdir")) {
		toolResp += await mkdir(
			validator<"mkdir">(toolCall).arguments,
		);
	} else if (toolCall.includes("lnSym")) {
		toolResp += await lnSym(
			validator<"lnSym">(toolCall).arguments,
		);
	} else if (toolCall.includes("mv")) {
		toolResp += await mv(
			validator<"mv">(toolCall).arguments,
		);
	} else if (toolCall.includes("rmRf")) {
		toolResp += await rmRf(
			validator<"rmRf">(toolCall).arguments,
		);
	} else if (toolCall.includes("inspect")) {
		toolResp += await inspect(
			validator<"inspect">(toolCall).arguments,
		);
	}
}
const question = context.trim();
const user = question !== "" ? await dmenuAsk({ question }) : "";

const assistant = "<tool_call>" +
	toolCalls.join("</tool_call>\n<tool_call>") +
	"</tool_call>";

ifc.pushAndReturn({ context, assistant, tool: toolResp, user });
