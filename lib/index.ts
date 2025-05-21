import { Type as t } from "npm:@sinclair/typebox";
import type {
	TLiteral,
	TObject,
	TOptional,
	TString,
} from "npm:@sinclair/typebox";
import { Value as v } from "npm:@sinclair/typebox/value";
import { pipeLazy } from "npm:@fxts/core";

const Params = (
	defaults: {
		system?: string;
		base_url?: string;
		model?: string;
		api_token?: string;
	},
) => t.Object({
	system: t.String({ default: defaults.system }),
	context: t.Array(t.String()),
	assistant: t.Array(t.String()),
	tool: t.Array(t.String()),
	user: t.Array(t.String()),
	init_status: t.Optional(t.String()),
	status: t.Array(t.String()),
	base_url: t.String({ default: defaults.base_url }),
	model: t.String({ default: defaults.model }),
	to_agent: t.String(),
	from_agent: t.Optional(t.String()),
	api_token: t.Optional(t.String({ default: defaults.api_token })),
	tool_calls: t.Optional(t.Array(t.Any())),
	response: t.Optional(t.String()),
});

export const schema = <
	K extends string,
	T extends {
		[F in K]: { [A in string | 0]: string };
	},
>(body: T) => {
	const validatorOpts: TObject<
		{
			name: TLiteral<Extract<keyof T, string>>;
			arguments: TObject<{ [P in string]: TOptional<TString> }>;
		}
	>[] = [];
	const oaiSpec = [];
	for (const key in body) {
		const value = body[key];
		const args = t.Object(
			Object.fromEntries(
				Object.entries(value).filter(([name, _]) => name != "0")
					.map((
						[name, description],
					) => [name, t.Optional(t.String({ description }))]),
			),
			{ description: value[0] },
		);
		oaiSpec.push({ name: key, arguments: args });
		validatorOpts.push(t.Object({ name: t.Literal(key), arguments: args }));
	}
	return [
		oaiSpec,
		<N extends keyof T>(
			str: string,
		): { name: N; arguments: T[N] } =>
			v.Parse(t.Union(validatorOpts), JSON.parse(str)),
	] as const;
};

export class Interface {
	private params: ReturnType<typeof Params>["static"];

	static async withDefaults(
		defaults: {
			system?: string;
			base_url?: string;
			model?: string;
			api_token?: string;
		},
	) {
		return new Interface(v.Parse(
			Params(defaults),
			await new Response(Deno.stdin.readable).json(),
		));
	}

	constructor(params: ReturnType<typeof Params>["static"]) {
		this.params = params;
	}

	get response() {
		return this.params.response ?? "";
	}

	keepAndReturn(initStatus?: string) {
		if (initStatus) this.params.init_status = initStatus;
		console.log(JSON.stringify(this.params));
		Deno.exit(0);
	}

	pushAndReturn(
		{ context, assistant, tool, user, status, tool_calls }: {
			context: string;
			assistant: string;
			tool: string;
			user?: string;
			status?: string;
			tool_calls?: object[];
		},
		pruneLimit = 1e5,
	) {
		user ??= "";
		status ??= "";
		let totalLength = context.length + assistant.length + tool.length +
			user.length + status.length;
		for (let i = this.params.tool.length - 1; i >= 0; i--) {
			totalLength += this.params.context[i].length +
				this.params.assistant[i].length +
				this.params.tool[i].length +
				this.params.user[i].length +
				this.params.status[i].length;
			if (totalLength > pruneLimit) {
				this.params.context = this.params.context.slice(i + 1);
				this.params.assistant = this.params.assistant.slice(i + 1);
				this.params.tool = this.params.tool.slice(i + 1);
				this.params.user = this.params.user.slice(i + 1);
				this.params.status = this.params.status.slice(i + 1);
				break;
			}
		}
		this.params.context.push(context);
		this.params.assistant.push(assistant);
		this.params.tool.push(tool);
		this.params.user.push(user);
		this.params.status.push(status);
		this.params.tool_calls = tool_calls;
		this.params.init_status = undefined;
		console.log(JSON.stringify(this.params));
		Deno.exit(0);
	}

	clearTools() {
		this.params.tool = this.params.tool.map((_) => "");
	}

	clearStatus() {
		this.params.status = this.params.status.map((_) => "");
	}
}

export const insertContent = (filePath: string, content: string) => {
	const i = filePath.lastIndexOf("/");
	const dir = filePath.slice(0, i);
	Deno.mkdirSync(dir, { recursive: true });
	Deno.writeTextFileSync(filePath, content);
	return dir;
};

type Parser<Curr, Next> = (input: [string, Curr]) => [string, Next];

export const seq = pipeLazy;

export function takeAndSkip<Curr, K extends string>(
	key: K,
	to: string,
): Parser<Curr, Curr & { [P in K]?: string }> {
	return ([input, state]) => {
		const idx = input.indexOf(to);
		if (idx === -1) throw [input, state];
		return [input.slice(idx + to.length), {
			...state,
			[key]: input.slice(0, idx),
		}];
	};
}

export function takeAndSkipMany<
	Curr,
	K extends string,
>(
	key: K,
	to: string,
	extra: Parser<Curr, Curr>,
): Parser<Curr, Curr & { [P in K]?: string[] }> {
	return ([input, state]) => {
		let rem = input;
		const arr = [];
		while (true) {
			try {
				const [inn, out] = takeAndSkip(key, to)([rem, {}]);
				rem = inn;
				if (out[key]) arr.push(out[key]);
				try {
					rem = extra([rem, state])[0];
				} catch (_) {
					_;
				}
			} catch (_) {
				return [rem, { ...state, [key]: arr }];
			}
		}
	};
}

export function takeAllOrSkip<Curr, K extends string>(
	key: K,
	to: string,
): Parser<Curr, Curr & { [P in K]?: string }> {
	return ([input, state]) => {
		const idx = input.indexOf(to);
		const end = idx === -1 ? input.length : idx;
		const remainingInput = input.slice(end + (idx === -1 ? 0 : to.length));
		return [remainingInput, { ...state, [key]: input.slice(0, end) }];
	};
}

export function whitespace<Curr>(
	[input, state]: [string, Curr],
): [string, Curr] {
	return [input.trimStart(), state];
}

export function opt<Curr, Next>(
	inner: Parser<Curr, Next>,
): Parser<Curr, Curr | Next> {
	return ([input, state]: [string, Curr]) => {
		try {
			return inner([input, state]);
		} catch (_err) {
			return [input, state];
		}
	};
}

export function literal<Curr>(str: string): Parser<Curr, Curr> {
	return ([input, state]) => {
		if (!input.startsWith(str)) throw [input, state];
		return [input.slice(str.length), state];
	};
}
