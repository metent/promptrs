//! Core client implementation for chat completions.

use attohttpc::body::Json;
use attohttpc::header::CONTENT_TYPE;
use attohttpc::{Error, ResponseReader, TextReader};
use either::IntoEither;
use log::info;
use serde::ser::SerializeSeq;
use serde::{Deserialize, Serialize, Serializer};
use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Lines};
use std::iter::StepBy;
use std::time::Duration;

/// Represents a chat completion request configuration.
pub struct Request<'s> {
	pub api_key: &'s Option<String>,
	pub base_url: &'s String,
	pub body: Params<'s>,
}

impl Request<'_> {
	/// Sends the chat completion request and processes streaming response.
	pub fn chat_completion(
		&self,
		mut on_token: Option<impl FnMut(String)>,
	) -> Result<Response, Error> {
		let mut buf = String::new();
		let mut tool_calls = Vec::new();
		let content = self.stream()?.fold(String::new(), |acc, chunk| {
			let Ok(chunk) = chunk else {
				return acc;
			};

			let Some(Choice {
				delta: Delta {
					content,
					tool_calls: tcs,
				},
			}) = chunk.choices.into_iter().next()
			else {
				return acc;
			};

			if let Some(tcs) = tcs {
				tool_calls.extend(tcs.into_iter().map(|tc| Function {
					name: tc.function.name,
					arguments: tc.function.arguments,
				}));
			}

			let Some(text) = content else { return acc };
			buf += &text;
			while let Some(end) = buf.find('\n') {
				info!("{}", &buf[..end]);
				buf = buf.split_off(end + 1);
			}

			let result = acc + text.as_str();
			if let Some(f) = on_token.as_mut() {
				f(text)
			}
			result
		});

		Ok(Response {
			reasoning: None,
			content,
			tool_calls,
		})
	}

	fn stream(&self) -> Result<ChatCompletionStream, Error> {
		let (status, _, reader) =
			attohttpc::post(self.base_url.to_string() + "/v1/chat/completions")
				.read_timeout(Duration::MAX)
				.into_either(self.api_key.is_some())
				.right_or_else(|req| req.bearer_auth(self.api_key.as_ref().as_slice()[0]))
				.header(CONTENT_TYPE, "application/json")
				.body(Json(&self.body))
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
	type Item = Result<ChatCompletionChunk, io::Error>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.and_then(|s| s.and_then(next_chunk).transpose())
	}
}

fn next_chunk(mut s: String) -> Result<Option<ChatCompletionChunk>, io::Error> {
	if s.contains("[DONE]") {
		return Ok(None);
	}
	if let Some(i) = s.find('{') {
		s = s.split_off(i);
	}
	serde_json::from_str(&s)
		.map_err(|err| io::Error::new(io::ErrorKind::InvalidData, format!("Malformed JSON: {err}")))
}

/// API request parameters
#[derive(Serialize)]
pub struct Params<'s> {
	pub model: &'s String,
	pub temperature: Option<f64>,
	pub top_p: Option<f64>,
	#[serde(serialize_with = "serialize_messages")]
	pub messages: &'s [Message],
	pub stream: bool,
	#[serde(flatten)]
	pub extra: &'s serde_json::Map<String, serde_json::Value>,
}

/// Represents a chat message in conversation history.
#[derive(Clone, Debug)]
pub enum Message {
	/// Instructions for assistant behavior
	System(String),
	/// Direct user messages
	User(String),
	/// LLM responses without reasoning
	Assistant(String),
	/// Tool call identifier, arguments and response
	ToolCall((String, String)),
	/// Status tool call identifer and response
	Status((String, String)),
}

fn serialize_messages<S: Serializer>(
	messages: &[Message],
	serializer: S,
) -> Result<S::Ok, S::Error> {
	let mut seq = serializer.serialize_seq(Some(messages.len()))?;

	let mut messages = messages.iter();
	let mut curr = messages.next();

	loop {
		while let Some(Message::ToolCall((assistant, tool)) | Message::Status((assistant, tool))) =
			curr
		{
			seq.serialize_element(&HashMap::from([
				("role", "assistant"),
				("content", assistant),
			]))?;
			seq.serialize_element(&HashMap::from([("role", "tool"), ("content", tool)]))?;
			curr = messages.next();
		}

		let Some(message) = curr else { break };

		let (role, content) = match message {
			Message::System(content) => ("system", content),
			Message::User(content) => {
				if let Some(Message::Status((_, status))) = curr {
					let user = &format!("{content}\n\nCurrent Status is\n\n{status}");
					let map = HashMap::from([("role", "user"), ("content", user)]);
					seq.serialize_element(&map)?;

					messages.next();
					curr = messages.next();

					continue;
				}
				("user", content)
			}
			Message::Assistant(content) => ("assistant", content),
			_ => unreachable!(),
		};
		let map = HashMap::from([("role", role), ("content", content)]);
		seq.serialize_element(&map)?;

		curr = messages.next();
	}
	seq.end()
}

/// Response structure containing AI output and tool calls.
///
/// # Fields
/// * `reasoning` - Optional assistant reasoning about response generation
/// * `content` - Main response content from AI
/// * `tool_calls` - List of executed tool function calls
pub struct Response {
	pub reasoning: Option<String>,
	pub content: String,
	pub tool_calls: Vec<Function>,
}

#[derive(Debug, Deserialize)]
pub struct ChatCompletionChunk {
	choices: Vec<Choice>,
}

#[derive(Debug, Deserialize)]
struct Choice {
	#[serde(alias = "message")]
	delta: Delta,
}

#[derive(Debug, Deserialize)]
struct Delta {
	content: Option<String>,
	tool_calls: Option<Vec<ToolCall>>,
}

#[derive(Clone, Debug, Deserialize)]
struct ToolCall {
	function: Function,
}

/// Represents a function call to an external tool.
#[derive(Clone, Debug, Deserialize)]
pub struct Function {
	/// Name of the tool function to invoke
	pub name: String,
	/// Arguments for the tool in JSON format
	pub arguments: Arguments,
}

/// Tool call arguments
pub type Arguments = serde_json::Map<String, serde_json::Value>;
