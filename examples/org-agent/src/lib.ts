import {
  literal,
  opt,
  seq,
  takeAllOrSkip,
  takeAndSkip,
  takeAndSkipMany,
} from "./parse.ts";
import { OrgTools } from "./tools.ts";
import { assistant, status, tool } from "./util.ts";
import type { GeneratorIfc } from "promptrs:gen/ifc";

class Generator extends OrgTools implements GeneratorIfc {
  constructor(offset = 0) {
    super();
    this.offset = offset;
  }

  init() {
    const system = `
You are an autonomous task organizer managing an org-mode file for the user's tasks and knowledge base. Your primary goal is to process inputs, gather knowledge using NOTEs, structure TASKs, and make decisions without unnecessary user interruptions. You must ask questions from the user only when there are no more operations to perform without ambiguity. The user will provide rough inputs, but you need to label them as either knowledge(using NOTE) or tasks(using TASK) and organize them in the task hierarchy.

# Task Processing Rules
- When sufficient information is available, directly execute task operations using tools, otherwise ask questions from the user. You must not ask questions like "What priority should I assign to this task?" or "What should be the scheduled date for this task?". If possible, schedule them using available knowledge, otherwise do not assign these properties.
- Your questions must primarily be related to the user or the people they are associated with. Build the knowledge base using the replies. Continue adding tasks/other knowledge if the user ignores the question.
- Only request clarification if there is some ambiguity (e.g. Which project are you referring to?)
- Based on whatever is known about the user, automatically assign properties like priorities, but ask if the user is comfortable with the suggested start date, due date or deadline.
- Use human-readable IDs for adding tasks such as 'buyGroceries' or 'playTennis'. Keep the IDs short (less than 15 characters).

# Tools

You may call one or more functions to assist with the user query.

You are provided with function signatures within <tools></tools> XML tags:
<tools>
${JSON.stringify(this.oaiSpec)}
</tools>

For each function call, return a json object with function name and arguments within <tool_call></tool_call> XML tags:
<tool_call>
{"name": <function-name>, "arguments": <args-json-object>}
</tool_call>
You may make multiple tool calls in a single response.
`;
    const status = this.getStatus();
    return {
      baseUrl: "https://192.168.1.36",
      model: "Qwen3",
      charLimit: 300_000n,
      temperature: 0.6,
      topP: 0.95,
      system,
      user: this.processInstructions(status),
      status,
    };
  }

  build(response: string) {
    let [_, { context, calls }] = seq(
      opt(takeAndSkip("_", "</think>")),
      takeAllOrSkip("context", "<tool_call>"),
      opt(literal("\n")),
      takeAndSkipMany(
        "calls",
        "</tool_call>",
        seq(
          takeAndSkip("context", "<tool_call>"),
          opt(literal("\n")),
        ),
      ),
      takeAllOrSkip("context", "<tool_call>"),
    )([response, {}]);
    context = context ?? "";
    calls = calls ?? [];

    const toolCalls = [];
    const toolResps = [];
    for (const toolCall of calls) {
      const { name, arguments: args }: { name: string; arguments: never } = JSON
        .parse(toolCall);
      if (!this.tools[name]) continue;

      toolCalls.push(toolCall);
      toolResps.push(this.tools[name](args));
    }

    let user;
    if (
      context.includes("?") || context.includes("Please") ||
      context.includes("please")
    ) {
      user = this.promptUser(context);
    } else if (!toolCalls.length) {
      user = this.processInstructions(context);
    }

    return {
      ...assistant([context, "<tool_call>\n", toolCalls, "</tool_call>\n"]),
      ...tool(["<tool_response>", toolResps, "</tool_response>\n"]),
      ...status([
        JSON.stringify({ name: "getStatus", arguments: {} }),
        this.getStatus(),
      ]),
      user,
    };
  }
}

export const chat = { Generator };
