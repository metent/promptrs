use crate::openai::{Chat, Client, Message, Text};
use exports::promptrs::r#gen::chat::{GuestGenerator, Msg, Sys};
use iana_time_zone::get_timezone;
use jiff::Timestamp;
use jiff::tz::TimeZone;
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
	char_count: usize,
	char_limit: usize,
	status_idx: Option<(usize, usize)>,
	user_status_idx: Option<usize>,
}

impl<'i> Prompter<'i> {
	pub fn new(ifc: &'i Ifc, mut store: Store<ComponentRunStates>) -> Result<Self> {
		let generator = ifc.promptrs_gen_chat().generator();
		let tz = TimeZone::get(&get_timezone()?)?.to_offset(Timestamp::now());
		let resource = generator.call_constructor(&mut store, Some(tz.seconds()))?;
		Ok(Prompter {
			generator,
			resource,
			store,
			char_count: 0,
			char_limit: 0,
			status_idx: None,
			user_status_idx: None,
		})
	}

	pub fn init(&mut self) -> Result<Client> {
		let Sys {
			base_url,
			api_key,
			model,
			char_limit,
			temperature,
			top_p,
			system,
			user,
			status,
		} = self.generator.call_init(&mut self.store, self.resource)??;

		self.char_limit = usize::try_from(char_limit)?;
		if status.is_some() {
			self.user_status_idx = Some(1);
		}
		Ok(Client {
			chat: Chat {
				model,
				temperature,
				top_p,
				messages: vec![
					Message::System { content: system },
					Message::User {
						content: std::iter::once(user)
							.chain(status.into_iter())
							.map(|text| Text { text })
							.collect(),
					},
				],
				tools: None,
				stream: true,
			},
			api_key,
			base_url,
		})
	}

	pub fn build(&mut self, client: &mut Client, response: &str) -> Result<()> {
		let Msg {
			assistant: (context, tc_open, calls, tc_close),
			tool: (tr_open, resps, tr_close),
			tool_call_id,
			user,
			status,
		} = self
			.generator
			.call_build(&mut self.store, self.resource, response)??;
		let mut messages = &mut client.chat.messages;
		let (status_tc, status_tr) = status.unzip();

		self.status_idx = match status_tc {
			Some(_) => {
				self.clear_status(&mut messages);
				Some((calls.len() + 1, resps.len()))
			}
			None => None,
		};

		let tool_calls = calls
			.iter()
			.chain(status_tc.iter())
			.map(|tc| format!("{tc_open}{tc}{tc_close}"));
		let assistant = std::iter::once(context)
			.chain(tool_calls)
			.map(|text| Text { text })
			.collect::<Vec<_>>();
		self.char_count += assistant.iter().fold(0, |acc, t| acc + t.text.len());
		messages.push(Message::Assistant { content: assistant });

		let tool = resps
			.iter()
			.chain(status_tr.iter())
			.map(|tr| Text {
				text: format!("{tr_open}{tr}{tr_close}"),
			})
			.collect::<Vec<_>>();
		self.char_count += tool.iter().fold(0, |acc, t| acc + t.text.len());
		if !tool.is_empty() {
			messages.push(Message::Tool {
				content: tool,
				tool_call_id: tool_call_id.unwrap_or("last".into()),
			});
		}

		if let Some(user) = user {
			self.char_count += user.len();
			messages.push(Message::User {
				content: vec![Text { text: user }],
			});
		}

		self.prune_history(messages);

		Ok(())
	}

	fn clear_status(&mut self, messages: &mut Vec<Message>) {
		let Some((tc, tr)) = self.status_idx else {
			if let Message::User { content } = &mut messages[1] {
				if let Some(idx) = self.user_status_idx {
					content.remove(idx);
					self.user_status_idx = None;
				}
			}
			return;
		};
		let mut flag = 0;
		let mut rm_idx = None;

		for (i, message) in messages.iter_mut().enumerate().rev() {
			match message {
				Message::Tool { content, .. } if flag == 0 => {
					content.remove(tr);
					if content.is_empty() {
						rm_idx = Some(i);
					}
					flag += 1;
				}
				Message::Assistant { content } if flag == 1 => {
					content.remove(tc);
					flag += 1;
				}
				_ if flag == 2 => break,
				_ => {}
			}
		}

		if let Some(i) = rm_idx {
			messages.remove(i);
		}
		self.status_idx = None;
	}

	fn prune_history(&mut self, messages: &mut Vec<Message>) {
		if self.char_count < self.char_limit {
			return;
		}

		let mut prune_count = 0;
		let mut prune_till = None;
		for (i, message) in messages[2..].iter().enumerate() {
			match message {
				Message::User { content } => {
					prune_count += content.iter().fold(0, |acc, t| acc + t.text.len())
				}
				Message::Assistant { content } => {
					prune_count += content.iter().fold(0, |acc, t| acc + t.text.len())
				}
				Message::Tool { content, .. } => {
					prune_count += content.iter().fold(0, |acc, t| acc + t.text.len())
				}
				_ => {}
			}
			if self.char_count - prune_count < self.char_limit {
				prune_till = Some(i);
				break;
			}
		}

		if let Some(i) = prune_till {
			messages.drain(..=i);
			self.char_count -= prune_count;
		}
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
