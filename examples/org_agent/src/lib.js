import {
	literal,
	opt,
	seq,
	takeAllOrSkip,
	takeAndSkip,
	takeAndSkipMany,
} from "./parse.js";
import t, { getStatus, processInstructions } from "./tools.js";

/** @returns {{model: string, system: string, user: string}} */
export function init() {
	const system = `
You are an autonomous task organizer managing an org-mode file. Your primary goal is to process inputs, structure tasks, and make decisions without unnecessary user interruptions. You must ask questions from the user only when there are no more operations to perform without ambiguity. The user will provide rough inputs, but you need to properly write them and deduce the parent tasks to organize them in the task hierarchy.

# Task Processing Rules
- When sufficient information is available, directly execute task operations using tools, otherwise ask questions from the user
- Your questions must primarily be related to the user or the people they are associated with. Build the knowledge base using the replies. Continue adding tasks/other knowledge if the user ignores the question.
- Only request clarification if there is some ambiguity (e.g. Which parent task does this subtask belong to?)
- Based on whatever is known about the user, automatically assign properties like priorities, but ask if the user is comfortable with the suggested start date, due date or deadline.
- Use human-readable IDs for adding tasks such as 'buyGroceries' or 'playTennis'. Keep the IDs short (less than 15 characters).

# Tools
You may call one or more functions to assist with the user query. Function signatures:
\`\`\`json
${JSON.stringify(t.oaiSpec)}
\`\`\`

For each function call, return a json object with function name and arguments within code blocks:
\`\`\`json
{"name": <function-name>, "arguments": <args-json-object>}
\`\`\`
You may make multiple tool calls in a single response.
`;
	return { model: "xLAM-2", system, user: "", baseUrl: "https://192.168.1.35" };
}

/**
 * @param {string} response
 * @returns {{assistant: string, tool?: string, tool_call_id?: string, user?: string}}
 */
export function build(response) {
	let [_, { context, toolCalls }] = seq(
		takeAllOrSkip("context", "```"),
		opt(literal("json")),
		opt(literal("\n")),
		takeAndSkipMany(
			"toolCalls",
			"```",
			seq(
				takeAndSkip("_", "```"),
				opt(literal("json")),
				literal("\n"),
			),
		),
	)([response, {}]);
	context = context ?? "";
	toolCalls = toolCalls ?? [];

	let toolResp = "";
	for (const toolCall of toolCalls) {
		const { name, arguments: args } = JSON.parse(toolCall);
		toolResp += t.tools[name](args);
	}

	const question = context.trim();
	let user = getStatus();
	user += question !== "" ? processInstructions() : "";

	return { assistant: response, tool: toolResp, toolCallId: "", user };
}
