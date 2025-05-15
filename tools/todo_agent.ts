import { Type as t } from "npm:@sinclair/typebox";
import { Value as v } from "npm:@sinclair/typebox/value";

const ToolCalls = t.Union([
	t.Object({
		name: t.Literal("insertContent"),
		arguments: t.Object({
			filePath: t.String({
				description:
					"Path of file in which content will be inserted (truncated). If file is absent, it will be automatically created before insertion",
			}),
			sourceCode: t.String({
				description:
					"Source code content which will be used to truncate the file.",
			}),
		}),
	}, {
		description:
			"Creates a file if does not exist and writes or overwrites content to it",
	}),
	t.Object({
		name: t.Literal("cargoCheck"),
		arguments: t.Object({}),
	}, {
		description: "Compiles the project and reports errors if present",
	}),
]);

const SYSTEM = `
You are an assistant who is writing a command line to-do app in rust.

Here is the description of the format in which your responses must be structured. You must first provide documentation of all APIs implemented till now.
Then you must provide the context of the current issue or the implementation you are working on and write the API interfaces (without function bodies) that you have created till now.

# API Interfaces:
- \`fn some_mod::some_func1(&self, task: &str) -> Result<(), String>\`: Placeholder Description
- \`fn some_mod::some_func2(&self, task_id: usize) -> Result<(), String>\`: Placeholder Description.
- \`fn some_other_operation ...\`

# Context: Provide relevant information regarding the current task.

# Tools

You may call one or more functions to assist with the user query.

You are provided with function signatures within <tools></tools> XML tags:
<tools>
${JSON.stringify(ToolCalls)}
</tools>

For each function call, return a json object with function name and arguments within <tool_call></tool_call> XML tags:
<tool_call>
{"name": <function-name>, "arguments": <args-json-object>}
</tool_call>

/no_think
`;

const Params = t.Object({
	system: t.String({ default: SYSTEM }),
	context: t.Array(t.String()),
	assistant: t.Array(t.String()),
	tool: t.Array(t.String()),
	base_url: t.String({ default: "https://192.168.1.35" }),
	model: t.String({ default: "Qwen3-A3B" }),
	to_agent: t.String(),
	from_agent: t.Optional(t.String()),
	response: t.Optional(t.String()),
});
const params = v.Parse(Params, await new Response(Deno.stdin.readable).json());

let think = params.response?.indexOf("</think>");
if (think === -1) think = undefined;

let start = params.response?.indexOf("<tool_call>");
if (start === -1) start = undefined;

const context = params.response?.slice(think && think + 8, start);
if (!context || context === "undefined") {
	(console.log(JSON.stringify(params)), Deno.exit(0));
}
if (!start || !params.response) {
	const last = params.context.length - 1;
	if (last >= 0) params.context[last] = context;
	(console.log(JSON.stringify(params)), Deno.exit(0));
}

const callTool = {
	insertContent: async (
		{ filePath, sourceCode }: { filePath: string; sourceCode: string },
	) => {
		try {
			await Deno.stat("Cargo.toml").catch(() =>
				new Deno.Command("cargo", { args: ["init"] }).output()
			);

			const i = filePath.lastIndexOf("/");
			const dir = filePath.slice(0, i);
			const module = filePath.slice(i + 1).split(".rs")[0];
			Deno.mkdirSync(dir, { recursive: true });
			if (dir !== "src") {
				const modText = Deno.readTextFileSync(`${dir}/mod.rs`);
				Deno.writeTextFileSync(
					`${dir}/mod.rs`,
					`mod ${module};\n${modText}`,
				);
			} else {
				const mainText = Deno.readTextFileSync(`${dir}/main.rs`);
				Deno.writeTextFileSync(
					`${dir}/main.rs`,
					`mod ${module};\n${mainText}`,
				);
			}
			Deno.writeTextFileSync(filePath, sourceCode);
			return "<tool_response>Inserted Successfully</tool_response>\n";
		} catch (err) {
			return `<tool_response>${JSON.stringify(err)}</tool_response>\n`;
		}
	},
	cargoCheck: async ({}: Record<string | number | symbol, never>) => {
		try {
			const output = await new Deno.Command("cargo", {
				args: ["check", "--message-format", "short"],
			})
				.output();
			return `<tool_response>${
				JSON.stringify({
					...output,
					stdout: await new Response(output.stdout).text(),
					stderr: await new Response(output.stderr).text(),
				})
			}</tool_response>\n`;
		} catch (err) {
			return `<tool_response>${JSON.stringify(err)}</tool_response>\n`;
		}
	},
};

let rem = params.response;
let tool = "";
while (true) {
	const start = rem.indexOf("<tool_call>");
	if (start == -1) break;
	rem = rem.slice(start + 11);

	const end = rem.indexOf("</tool_call>");
	const toolCall = rem.slice(0, end === -1 ? undefined : end).trim()
		.replaceAll("\n", "\\n");
	if (!toolCall) break;
	const assistant = v.Parse(ToolCalls, JSON.parse(toolCall));
	switch (assistant.name) {
		case "insertContent":
			tool += await callTool[assistant.name](assistant.arguments);
			break;
		case "cargoCheck":
			tool += await callTool[assistant.name](assistant.arguments);
	}
}

params.context.push(context);
params.assistant.push(params.response.slice(start + 11));
if (tool) params.tool = params.tool.map((_) => "");
params.tool.push(tool);

console.log(JSON.stringify(params));
