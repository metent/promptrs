use crate::openai::Message;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::io::Result;

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct Prompt {
	#[serde(skip_serializing_if = "Option::is_none")]
	pub system: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub response: Option<String>,
	pub context: Vec<String>,
	pub assistant: Vec<String>,
	pub tool: Vec<String>,
	pub user: Vec<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub init_status: Option<String>,
	pub status: Vec<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub base_url: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub model: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub api_token: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub from_agent: Option<String>,
	pub to_agent: String,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub tool_calls: Option<Vec<Map<String, Value>>>,
}

pub trait Prompter {
	fn run(&self, prompt: &Prompt) -> Result<Prompt>;

	fn messages<'p>(&self, prompt: &'p Prompt) -> Vec<Message<'p>> {
		let iter = prompt
			.context
			.iter()
			.zip(prompt.tool.iter())
			.zip(prompt.user.iter())
			.zip(prompt.status.iter())
			.zip(prompt.assistant.iter())
			.map(|((((ctx, tool), user), status), assistant)| {
				[
					Some(Message::Assistant {
						content: ctx.to_string() + assistant,
					}),
					tool.ne("").then_some(Message::Tool {
						tool_call_id: "last",
						content: tool,
					}),
					(user.ne("") || status.ne("")).then_some(Message::User {
						content: status.to_string() + user,
					}),
				]
			})
			.flatten()
			.flatten();
		prompt
			.system
			.iter()
			.map(|sys| {
				[
					Message::System { content: sys },
					prompt
						.init_status
						.as_ref()
						.map(|status| Message::User {
							content: status.to_string(),
						})
						.unwrap_or(Message::User { content: "".into() }),
				]
			})
			.flatten()
			.chain(iter)
			.collect()
	}
}
