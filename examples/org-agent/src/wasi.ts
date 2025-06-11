import { subscribeDuration } from "wasi:clocks/monotonic-clock@0.2.5";

export function sleep(seconds: bigint) {
  subscribeDuration(seconds * 1_000_000_000n).block();
}
