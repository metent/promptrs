use crate::prompt::{ComponentRunStates, WasmPrompter};
use log::{info, warn};
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;
use wasmtime::component::{Component, Linker};
use wasmtime::{Engine, Error, Result, Store};

pub struct ChatLoop {
	store: Store<ComponentRunStates>,
	prompter: WasmPrompter,
}

impl ChatLoop {
	pub fn new(
		path: impl AsRef<Path>,
		host_path: impl AsRef<Path>,
		guest_path: impl AsRef<str>,
	) -> Result<Self, Error> {
		let engine = Engine::default();
		let component = Component::from_file(&engine, &path)?;

		let mut linker = Linker::new(&engine);
		wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
		WasmPrompter::add_to_linker(&mut linker, |state| state)?;

		let mut store = Store::new(&engine, ComponentRunStates::new(host_path, guest_path)?);
		let prompter = WasmPrompter::instantiate(&mut store, &component, &linker)?;

		Ok(ChatLoop { store, prompter })
	}

	pub fn process(mut self) -> Result<()> {
		let mut client = self.prompter.init(&mut self.store)?;

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

			if let Err(err) = self
				.prompter
				.build(&mut self.store, &mut client, &raw_response)
			{
				warn!("\n\nPrompt Generation Failed: {}", err);
			}
		}
	}
}
