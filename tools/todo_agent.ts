import {
	insertContent,
	Interface,
	schema,
	seq,
	takeAllOrSkip,
	takeAndSkip,
	takeAndSkipMany,
} from "../lib/index.ts";

const [oaiSpec, validator] = schema({
	inspect: {
		0: "Shows the current directory structure of the project, all public and private modules/functions/structs/enums/impls/traits, and any orphaned modules (if present)",
	},
	insertSource: {
		0: "Creates a file if does not exist and writes or overwrites content to it",
		filePath:
			"Path of file in which content will be inserted (truncated). If file is absent, it will be automatically created before insertion",
		sourceCode:
			"Source code content which will be used to truncate the file.",
	},
	cargoCheck: {
		0: "Compiles the project and reports errors if present",
	},
});

const system = `
You are an assistant who is writing a command line to-do app in rust.

Here is the description of the format in which your responses must be structured.
- You must first provide documentation of all APIs implemented till now.
- Next, you must provide all important facts you have learnt after receiving tool responses from your tool calls, which will be useful later on. **This should be EMPTY at the start and you must only add points here after tool calling.** Only 10 points are allowed at max, so remove the least important ones to make more space when required.
- Then you must provide the context of the current issue or the implementation you are working on and write the API interfaces (without function bodies) that you have created till now.

# API Interfaces:
- \`fn some_mod::some_func1(&self, task: &str) -> Result<(), String>\`: Placeholder Description
- \`fn some_mod::some_func2(&self, task_id: usize) -> Result<(), String>\`: Placeholder Description.
- \`fn some_other_operation ...\`

# Important
- This folder contains some_important_file.rs, I should later use it in dependent_module.rs
- ...

# Context: Provide relevant information regarding the current task.

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

/no_think
`;

const ifc = await Interface.withDefaults({
	system,
	base_url: "https://192.168.1.35",
	model: "Qwen3-A3B",
});

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

const insertSource = async (
	{ filePath, sourceCode }: { filePath: string; sourceCode: string },
) => {
	try {
		await Deno.stat("Cargo.toml").catch(() =>
			new Deno.Command("cargo", { args: ["init"] }).output()
		);

		const module =
			filePath.slice(filePath.lastIndexOf("/") + 1).split(".rs")[0];
		if (module === "lib") {
			return "<tool_response>Editing lib.rs not allowed</tool_response>\n";
		}
		const dir = insertContent(filePath, sourceCode);
		const moduleFile = dir === "src" ? "main.rs" : "mod.rs";
		const modText = Deno.readTextFileSync(`${dir}/${moduleFile}`);
		if (
			!filePath.endsWith(`/${moduleFile}`) &&
			!modText.includes(`mod ${module};`)
		) {
			Deno.writeTextFileSync(
				`${dir}/${moduleFile}`,
				`mod ${module};\n${modText}`,
			);
		}
		return "<tool_response>Inserted Successfully</tool_response>\n";
	} catch (err) {
		return `<tool_response>${JSON.stringify(err)}</tool_response>\n`;
	}
};

const cargoCheck = async ({}: object) => {
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
};

const inspect = async ({}: object) => {
	try {
		const fs = await new Deno.Command("erd", {
			args: ["-I", "--suppress-size", "src"],
			env: { NO_COLOR: "1" },
		})
			.output();
		const structure = await new Deno.Command("cargo", {
			args: ["modules", "structure"],
			env: { NO_COLOR: "1" },
		})
			.output();
		const orphans = await new Deno.Command("cargo", {
			args: ["modules", "orphans"],
			env: { NO_COLOR: "1" },
		})
			.output();
		return `<tool_response>${
			JSON.stringify({
				stdout: await new Response([
					fs.stdout,
					structure.stdout,
					orphans.stdout,
				]).text(),
				stderr: await new Response([
					fs.stderr,
					structure.stderr,
					orphans.stderr,
				]).text(),
			})
		}</tool_response>\n`;
	} catch (err) {
		return `<tool_response>${JSON.stringify(err)}</tool_response>\n`;
	}
};

let tool = "";
for (const toolCall of toolCalls) {
	if (toolCall.includes("insertSource")) {
		tool += await insertSource(
			validator<"insertSource">(toolCall).arguments,
		);
	} else if (toolCall.includes("cargoCheck")) {
		tool += await cargoCheck({});
	} else if (toolCall.includes("inspect")) {
		tool += await inspect({});
	}
}

const assistant = "<tool_call>" + toolCalls.join("</tool_call>\n<tool_call>") +
	"</tool_call>";

ifc.pushAndReturn({ context, assistant, tool });
