export interface Sys {
  baseUrl: string;
  model: string;
  system: string;
  user: string;
}

export interface Msg {
  assistant: string;
  tool?: string;
  toolCallId?: string;
  user?: string;
}
