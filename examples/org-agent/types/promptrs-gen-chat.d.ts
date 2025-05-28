export interface Sys {
  baseUrl: string;
  model: string;
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
