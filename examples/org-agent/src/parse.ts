import { pipeLazy } from "@fxts/core";

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
