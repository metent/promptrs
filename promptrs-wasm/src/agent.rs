use crate::prompt::{ImportChat, WasmPrompter};
use log::{info, warn};
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;
use wasmtime::component::{Component, Linker};
use wasmtime::{Engine, Error, Result, Store};

pub struct ChatLoop {
	store: Store<WasmPrompter>,
	bindings: ImportChat,
}

impl ChatLoop {
	pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
		let engine = Engine::default();
		let component = Component::from_file(&engine, &path)?;
		let mut linker = Linker::new(&engine);
		ImportChat::add_to_linker(&mut linker, |state| state)?;
		let mut store = Store::new(&engine, WasmPrompter::default());
		let bindings = ImportChat::instantiate(&mut store, &component, &linker)?;
		Ok(ChatLoop { store, bindings })
	}

	pub fn process(mut self) -> Result<()> {
		self.bindings.call_init(&mut self.store)?;
		let mut client = &self.store.data_mut().prompter()?.0;

		loop {
			info!("\n\nSending prompt: {:?}", client);
			let raw_response = match client.chat_completion() {
				Ok(res) => res,
				Err(err) => {
					warn!("\n\nChat Completion Failed: {}", err);
					sleep(Duration::from_secs(10));
					continue;
				}
			};

			if let Err(err) = self.bindings.call_build(&mut self.store, &raw_response) {
				warn!("\n\nPrompt Generation Failed: {}", err);
			}
			client = &self.store.data_mut().prompter()?.0;
		}
	}
}
