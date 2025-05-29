export interface Sys {
  baseUrl: string;
  apiKey?: string;
  model: string;
  charLimit: bigint;
  temperature?: number;
  topP?: number;
  system: string;
  user: string;
}

export interface Msg {
  assistant: [string, string, string[], string];
  tool: [string, string[], string];
  toolCallId?: string;
  status?: [string, string];
  user?: string;
}
