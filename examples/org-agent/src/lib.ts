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
You may call one or more functions to assist with the user query. Function signatures:
\`\`\`json
${JSON.stringify(this.oaiSpec)}
\`\`\`

For each function call, return a json object with function name and arguments within code blocks:
\`\`\`json
{"name": <function-name>, "arguments": <args-json-object>}
\`\`\`
You may make multiple tool calls in a single response.
`;
    const status = this.getStatus();
    return {
      baseUrl: "https://192.168.1.35",
      model: "xLAM-2",
      charLimit: 300_000n,
      system,
      user: this.processInstructions(status),
      status,
    };
  }

  build(response: string) {
    let [_, { context, calls }] = seq(
      takeAllOrSkip("context", "```"),
      opt(literal("json")),
      opt(literal("\n")),
      takeAndSkipMany(
        "calls",
        "```",
        seq(
          takeAndSkip("context", "```"),
          opt(literal("json")),
          literal("\n"),
        ),
      ),
      takeAllOrSkip("context", "```"),
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
      ...assistant([context, "```json\n", toolCalls, "```\n"]),
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
