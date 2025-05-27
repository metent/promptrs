use crate::openai::{Chat, Client, Message};
use exports::promptrs::r#gen::chat::{GuestGenerator, Msg, Sys};
use std::path::Path;
use wasmtime::component::{ResourceAny, ResourceTable, bindgen};
use wasmtime::{Result, Store};
use wasmtime_wasi::p2::{IoView, WasiCtx, WasiCtxBuilder, WasiView};
use wasmtime_wasi::{DirPerms, FilePerms};

bindgen!({ world: "ifc" });

pub struct Prompter<'i> {
	generator: GuestGenerator<'i>,
	resource: ResourceAny,
	store: Store<ComponentRunStates>,
}

impl<'i> Prompter<'i> {
	pub fn new(ifc: &'i Ifc, mut store: Store<ComponentRunStates>) -> Result<Self> {
		let generator = ifc.promptrs_gen_chat().generator();
		let resource = generator.call_constructor(&mut store)?;
		Ok(Prompter {
			generator,
			resource,
			store,
		})
	}

	pub fn init(&mut self) -> Result<Client> {
		let Sys {
			base_url,
			model,
			system,
			user,
		} = self.generator.call_init(&mut self.store, self.resource)?;
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

	pub fn build(&mut self, client: &mut Client, response: &str) -> Result<()> {
		let Msg {
			assistant,
			tool,
			tool_call_id,
			user,
		} = self
			.generator
			.call_build(&mut self.store, self.resource, response)?;
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

impl Drop for Prompter<'_> {
	fn drop(&mut self) {
		_ = self.resource.resource_drop(&mut self.store);
	}
}

pub struct ComponentRunStates {
	wasi_ctx: WasiCtx,
	resource_table: ResourceTable,
}

impl ComponentRunStates {
	pub fn new(host_dir: impl AsRef<Path>) -> Result<Self> {
		Ok(ComponentRunStates {
			wasi_ctx: WasiCtxBuilder::new()
				.preopened_dir(
					&host_dir,
					".",
					DirPerms::READ | DirPerms::MUTATE,
					FilePerms::READ | FilePerms::WRITE,
				)?
				.build(),
			resource_table: ResourceTable::new(),
		})
	}
}

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
