import { pipeLazy } from "@fxts/core";

export const insertContent = (filePath, content) => {
	const i = filePath.lastIndexOf("/");
	const dir = filePath.slice(0, i);
	Deno.mkdirSync(dir, { recursive: true });
	Deno.writeTextFileSync(filePath, content);
	return dir;
};

export const seq = pipeLazy;

export function takeAndSkip(key, to) {
	return ([input, state]) => {
		const idx = input.indexOf(to);
		if (idx === -1) throw [input, state];
		return [input.slice(idx + to.length), {
			...state,
			[key]: input.slice(0, idx),
		}];
	};
}

export function takeAndSkipMany(key, to, extra) {
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

export function takeAllOrSkip(key, to) {
	return ([input, state]) => {
		const idx = input.indexOf(to);
		const end = idx === -1 ? input.length : idx;
		const remainingInput = input.slice(end + (idx === -1 ? 0 : to.length));
		return [remainingInput, { ...state, [key]: input.slice(0, end) }];
	};
}

export function whitespace([input, state]) {
	return [input.trimStart(), state];
}

export function opt(inner) {
	return ([input, state]) => {
		try {
			return inner([input, state]);
		} catch (_err) {
			return [input, state];
		}
	};
}

export function literal(str) {
	return ([input, state]) => {
		if (!input.startsWith(str)) throw [input, state];
		return [input.slice(str.length), state];
	};
}
