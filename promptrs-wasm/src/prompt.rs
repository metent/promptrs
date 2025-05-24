use crate::openai::{Chat, Client, Message};
use openai::chat::chat::{Host, HostChatCompletion};
use wasmtime::component::{Resource, ResourceTable, bindgen};
use wasmtime::{Error, Result};

#[derive(Default)]
pub struct WasmPrompter {
	initialized: bool,
	table: ResourceTable,
}

impl WasmPrompter {
	pub fn prompter(&mut self) -> Result<&Prompter> {
		self.table
			.get_any_mut(0)?
			.downcast_ref()
			.ok_or(Error::msg("Chat not initiated"))
	}
}

impl Host for WasmPrompter {}

impl HostChatCompletion for WasmPrompter {
	fn new(&mut self, model: String, system: String, user: String) -> Result<Resource<Prompter>> {
		if self.initialized {
			return Err(Error::msg("Chat already initiated"));
		}

		let key = self.table.push(Prompter(Client {
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
		}))?;
		self.initialized = true;

		Ok(key)
	}

	fn generate(
		&mut self,
		prompter: Resource<Prompter>,
		assistant: String,
		tool: Option<String>,
		tool_call_id: Option<String>,
		user: Option<String>,
	) -> Result<()> {
		let messages = &mut self.table.get_mut(&prompter)?.0.chat.messages;
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

	fn drop(&mut self, _: Resource<Prompter>) -> Result<()> {
		Ok(())
	}
}

bindgen!({
	world: "import-chat",
	with: {
		"openai:chat/chat/chat-completion": Prompter,
	},
	trappable_imports: true,
});

pub struct Prompter(pub Client);
