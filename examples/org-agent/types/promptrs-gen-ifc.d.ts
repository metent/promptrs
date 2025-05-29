import type { Msg, Sys } from "./promptrs-gen-chat.d.ts";

export abstract class GeneratorIfc {
  constructor(timezone?: string);
  init(): Sys;
  build(response: string): Msg;
}

export { Msg, Sys };
