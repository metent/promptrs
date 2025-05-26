use crate::openai::{Chat, Client, Message};
use prompt::r#gen::chat::Host;
use std::path::Path;
use wasmtime::component::{ResourceTable, bindgen};
use wasmtime::{Result, Store};
use wasmtime_wasi::p2::{IoView, StdoutStream, WasiCtx, WasiCtxBuilder, WasiView};
use wasmtime_wasi::{DirPerms, FilePerms};
use wasmtime_wasi_http::{WasiHttpCtx, WasiHttpView};

bindgen!({ world: "wasm-prompter" });

impl WasmPrompter {
	pub fn init(&self, store: &mut Store<ComponentRunStates>) -> Result<Client> {
		let Sys {
			base_url,
			model,
			system,
			user,
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
			base_url,
		})
	}

	pub fn build(
		&self,
		store: &mut Store<ComponentRunStates>,
		client: &mut Client,
		response: &str,
	) -> Result<()> {
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

pub struct ComponentRunStates {
	wasi_ctx: WasiCtx,
	resource_table: ResourceTable,
	http_ctx: WasiHttpCtx,
}

impl ComponentRunStates {
	pub fn new(host_dir: impl AsRef<Path>) -> Result<Self> {
		Ok(ComponentRunStates {
			wasi_ctx: WasiCtxBuilder::new()
				.preopened_dir(
					&host_dir,
					".",
					DirPerms::MUTATE,
					FilePerms::READ | FilePerms::WRITE,
				)?
				.build(),
			resource_table: ResourceTable::new(),
			http_ctx: WasiHttpCtx::new(),
		})
	}
}

impl Host for ComponentRunStates {}

impl IoView for ComponentRunStates {
	fn table(&mut self) -> &mut ResourceTable {
		&mut self.resource_table
	}
}
impl WasiView for ComponentRunStates {
	fn ctx(&mut self) -> &mut WasiCtx {
		&mut self.wasi_ctx
	}
}

impl WasiHttpView for ComponentRunStates {
	fn ctx(&mut self) -> &mut WasiHttpCtx {
		&mut self.http_ctx
	}
}
