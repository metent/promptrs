use crate::openai::{Chat, Client, Message};
use prompt::r#gen::chat::Host;
use wasmtime::component::bindgen;
use wasmtime::{Result, Store};

bindgen!({ world: "wasm-prompter" });

impl WasmPrompter {
	pub fn init(&self, store: &mut Store<_0>) -> Result<Client> {
		let Sys {
			system,
			user,
			model,
		} = self.call_init(store)?;
		Ok(Client {
			chat: Chat {
				model,
				messages: vec![
					Message::System { content: system },
					Message::User { content: user },
				],
				tools: None,
				stream: true,
			},
			api_key: None,
			base_url: "".to_string(),
		})
	}

	pub fn build(&self, store: &mut Store<_0>, client: &mut Client, response: &str) -> Result<()> {
		let Msg {
			assistant,
			tool,
			tool_call_id,
			user,
		} = self.call_build(store, response)?;
		let messages = &mut client.chat.messages;

		messages.push(Message::Assistant { content: assistant });

		if let Some(tool) = tool {
			messages.push(Message::Tool {
				content: tool,
				tool_call_id: tool_call_id.unwrap_or("last".into()),
			});
		}

		if let Some(user) = user {
			messages.push(Message::User { content: user });
		}

		Ok(())
	}
}

pub struct _0;
impl Host for _0 {}
