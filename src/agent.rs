use crate::prompt::{ComponentRunStates, Ifc, Prompter};
use log::{info, warn};
use std::path::{Path, PathBuf};
use std::thread::sleep;
use std::time::Duration;
use wasmtime::component::{Component, Linker};
use wasmtime::{Cache, CacheConfig, Config, Engine, Error, Result, Store};

pub struct ChatLoop {
	store: Store<ComponentRunStates>,
	ifc: Ifc,
}

impl ChatLoop {
	pub fn new(
		path: impl AsRef<Path>,
		host_dir: impl AsRef<Path>,
		cache_dir: Option<PathBuf>,
	) -> Result<Self, Error> {
		let cache = cache_dir
			.map(|dir| {
				let mut cache_config = CacheConfig::new();
				cache_config.with_directory(dir);
				Cache::new(cache_config)
			})
			.transpose()?;
		let engine = Engine::new(Config::new().cache(cache))?;
		let component = Component::from_file(&engine, &path)?;

		let mut linker = Linker::new(&engine);
		wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;

		let mut store = Store::new(&engine, ComponentRunStates::new(host_dir)?);
		let ifc = Ifc::instantiate(&mut store, &component, &linker)?;

		Ok(ChatLoop { store, ifc })
	}

	pub fn process(mut self) -> Result<()> {
		let prompter = Prompter::new(&self.ifc, &mut self.store)?;
		let mut client = prompter.init(&mut self.store)?;

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

			if let Err(err) = prompter.build(&mut self.store, &mut client, &raw_response) {
				warn!("\n\nPrompt Generation Failed: {}", err);
			}
		}
	}
}
