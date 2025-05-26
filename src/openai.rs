use attohttpc::Error;
use attohttpc::body::Json;
use attohttpc::header::CONTENT_TYPE;
use attohttpc::{ResponseReader, TextReader};
use either::IntoEither;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::io::{self, BufRead, BufReader, Lines, Write};
use std::iter::StepBy;
use std::time::Duration;

#[derive(Debug)]
pub struct Client {
	pub chat: Chat,
	pub api_key: Option<String>,
	pub base_url: String,
}

impl Client {
	pub fn chat_completion(&self) -> Result<String, Error> {
		let response = self
			.stream()?
			.into_iter()
			.try_fold(String::new(), |acc, chunk| {
				let text = chunk?
					.choices
					.into_iter()
					.filter_map(|c| c.delta.content)
					.fold("".to_string(), |acc, s| acc + s.as_str());
				print!("{}", text);
				io::stdout().flush()?;
				Ok(acc + text.as_str())
			});
		println!("\n----------END_OF_RESPONSE----------\n\n");

		response
	}

	fn stream(&self) -> Result<ChatCompletionStream, Error> {
		let (status, _, reader) =
			attohttpc::post(self.base_url.to_string() + "/v1/chat/completions")
				.read_timeout(Duration::from_secs(600))
				.into_either(self.api_key.is_some())
				.right_or_else(|req| req.bearer_auth(self.api_key.as_ref().as_slice()[0]))
				.header(CONTENT_TYPE, "application/json")
				.body(Json(&self.chat))
				.send()?
				.split();
		if !status.is_success() {
			let resp = reader.text()?;
			return Err(Error::from(io::Error::new(
				io::ErrorKind::ConnectionRefused,
				format!("Status Code: {status}\nResponse: {resp}"),
			)));
		}

		Ok(ChatCompletionStream(
			BufReader::new(TextReader::new(reader, attohttpc::charsets::UTF_8))
				.lines()
				.step_by(2),
		))
	}
}

pub struct ChatCompletionStream(StepBy<Lines<BufReader<TextReader<ResponseReader>>>>);

impl Iterator for ChatCompletionStream {
	type Item = Result<ChatCompletionChunk, Error>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.filter(|l| !l.as_ref().is_ok_and(|l| l == "data: [DONE]"))?
			.ok()
			.filter(|l| l.len() > 6)
			.map(|l| {
				Ok(serde_json::from_str(&l[6..]).map_err(|err| {
					Error::from(io::Error::new(
						io::ErrorKind::InvalidData,
						format!("Malformed JSON: {err}"),
					))
				})?)
			})
	}
}

#[derive(Serialize, Debug)]
pub struct Chat {
	pub model: String,
	pub messages: Vec<Message>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub tools: Option<Vec<Map<String, Value>>>,
	pub stream: bool,
}

#[derive(Debug, Serialize, Clone)]
#[serde(tag = "role", rename_all = "snake_case")]
pub enum Message {
	System {
		content: String,
	},
	User {
		content: String,
	},
	Assistant {
		content: String,
	},
	Tool {
		content: String,
		tool_call_id: String,
	},
}

#[derive(Debug, Deserialize)]
pub struct ChatCompletionChunk {
	pub choices: Vec<Choice>,
}

#[derive(Debug, Deserialize)]
pub struct Choice {
	#[serde(alias = "message")]
	pub delta: Delta,
}

#[derive(Debug, Deserialize)]
pub struct Delta {
	pub content: Option<String>,
}
