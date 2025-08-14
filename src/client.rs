//! Core client implementation for chat completions.

use super::{SystemPromptMode, Tool};
use attohttpc::body::Json;
use attohttpc::header::CONTENT_TYPE;
use attohttpc::{Error, TextReader};
use either::IntoEither;
use log::info;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};
use serde_json::json;
use serde_json::value::RawValue;
use std::collections::HashMap;
use std::io::{self, BufRead, BufReader};
use std::time::Duration;

/// Represents a chat completion request configuration.
pub struct Request<'s, S> {
	pub api_key: &'s Option<String>,
	pub base_url: &'s String,
	pub body: Params<'s, S>,
}

impl<'s, S> Request<'s, S> {
	/// Sends the chat completion request and processes streaming response.
	pub fn chat_completion(
		&self,
		mut on_token: Option<impl FnMut(String, bool)>,
	) -> Result<Response, Error> {
		let mut buf = String::new();
		let mut rbuf = String::new();
		let mut tool_calls = Vec::new();
		let mut content = String::new();
		let mut reasoning = String::new();
		for chunk in self.stream()? {
			let Ok(chunk) = chunk else {
				continue;
			};

			let Some(Choice {
				delta:
					Delta {
						content: cnt,
						reasoning_content,
						tool_calls: tcs,
					},
			}) = chunk.choices.into_iter().next()
			else {
				continue;
			};

			if let Some(tcs) = tcs {
				tool_calls.extend(tcs.into_iter().map(|tc| Function {
					name: tc.function.name,
					arguments: tc.function.arguments,
				}));
			}

			if let Some(text) = cnt {
				buf += &text;
				while let Some(end) = buf.find('\n') {
					info!("{}", &buf[..end]);
					buf = buf.split_off(end + 1);
				}

				content.push_str(text.as_str());
				if let Some(f) = on_token.as_mut() {
					f(text, false)
				}
			}

			if let Some(text) = reasoning_content {
				rbuf += &text;
				while let Some(end) = rbuf.find('\n') {
					info!("{}", &rbuf[..end]);
					rbuf = rbuf.split_off(end + 1);
				}

				reasoning.push_str(text.as_str());
				if let Some(f) = on_token.as_mut() {
					f(text, true)
				}
			}
		}

		Ok(Response {
			reasoning: reasoning.len().gt(&0).then_some(reasoning),
			content,
			tool_calls,
		})
	}

	fn stream(
		&self,
	) -> Result<ChatCompletionStream<impl Iterator<Item = io::Result<String>>>, Error> {
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

pub struct ChatCompletionStream<T: Iterator<Item = io::Result<String>>>(T);

impl<T: Iterator<Item = io::Result<String>>> Iterator for ChatCompletionStream<T> {
	type Item = io::Result<ChatCompletionChunk>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.and_then(|s| s.and_then(next_chunk).transpose())
	}
}

fn next_chunk(mut s: String) -> io::Result<Option<ChatCompletionChunk>> {
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
pub struct Params<'s, S> {
	pub model: &'s String,
	pub temperature: Option<f64>,
	pub top_p: Option<f64>,
	pub inner: InnerParams<'s, S>,
	pub stream: bool,
	pub extra: &'s serde_json::Map<String, serde_json::Value>,
}

impl<S> Serialize for Params<'_, S> {
	fn serialize<R: Serializer>(&self, serializer: R) -> Result<R::Ok, R::Error> {
		let mut map = serializer.serialize_map(None)?;
		map.serialize_entry("model", self.model)?;
		map.serialize_entry("temperature", &self.temperature)?;
		map.serialize_entry("top_p", &self.top_p)?;
		map.serialize_entry("messages", &self.inner)?;
		let tools = self.inner.tools.iter().chain(self.inner.status.iter());
		map.serialize_entry("tools", &tools.collect::<Vec<_>>())?;
		map.serialize_entry("stream", &self.stream)?;
		for (key, value) in self.extra.iter() {
			map.serialize_entry(key, value)?;
		}
		map.end()
	}
}

pub struct InnerParams<'s, S> {
	pub messages: &'s [Message],
	pub tools: &'s [Tool<'s, S>],
	pub status: &'s Option<Tool<'s, S>>,
	pub mode: SystemPromptMode,
}

impl<S> Serialize for InnerParams<'_, S> {
	fn serialize<R: Serializer>(&self, serializer: R) -> Result<R::Ok, R::Error> {
		let InnerParams { messages, mode, .. } = self;
		let mut seq = serializer.serialize_seq(Some(messages.len()))?;

		let mut messages = messages.iter();
		let mut curr = messages.next();

		if let (Some(Message::System(system)), SystemPromptMode::User) = (curr, mode) {
			let mut content = system.clone();
			if let Some(Message::User(user)) = curr {
				content.push_str("\n\n");
				content.push_str(user);
			}
			seq.serialize_element(&HashMap::from([("role", "user"), ("content", &content)]))?;
			_ = messages.next();
			curr = messages.next();
		}

		loop {
			while let Some(
				Message::ToolCall((Function { name, arguments }, tool))
				| Message::Status((Function { name, arguments }, tool)),
			) = curr
			{
				let pair = (
					"tool_calls",
					json!([{
						"function": {
							"name": name,
							"arguments": serde_json::to_string(arguments)
								.unwrap_or_default(),
						},
						"id": "0",
						"type": "function",
					}]),
				);
				seq.serialize_element(&HashMap::from([("role", "assistant".into()), pair]))?;
				seq.serialize_element(&HashMap::from([("role", "tool"), ("content", tool)]))?;
				curr = messages.next();
			}

			let Some(message) = curr else { break };

			let (role, content) = match message {
				Message::System(content) => ("user", content),
				Message::User(content) => ("user", content),
				Message::Assistant(content) => ("assistant", content),
				_ => unreachable!(),
			};
			let map = HashMap::from([("role", role), ("content", content)]);
			seq.serialize_element(&map)?;

			curr = messages.next();
		}
		seq.end()
	}
}

impl<S> Serialize for Tool<'_, S> {
	fn serialize<R: Serializer>(&self, serializer: R) -> Result<R::Ok, R::Error> {
		let mut map = serializer.serialize_map(Some(1))?;
		let parameters =
			serde_json::from_str::<&RawValue>(self.jsonschema).unwrap_or(RawValue::NULL);
		map.serialize_entry("type", "function")?;
		map.serialize_entry(
			"function",
			&json!({
				"name": self.name,
				"description": self.description,
				"parameters": parameters,
				"strict": true,
			}),
		)?;
		map.end()
	}
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
	ToolCall((Function, String)),
	/// Status tool call identifer and response
	Status((Function, String)),
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

/// Response segment
pub enum Segment {
	/// Assistant reasoning about response generation
	Reasoning(String),
	/// Answer content
	Answer(String),
	/// Requested Tool Call
	ToolCall(Vec<Function>),
	/// Code block with optional language specifier
	CodeBlock(Option<String>, String),
	/// Any other response text
	Commentary(String),
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
	reasoning_content: Option<String>,
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
	#[serde(deserialize_with = "deserialize_arguments")]
	pub arguments: Arguments,
}

fn deserialize_arguments<'de, D: Deserializer<'de>>(
	deserializer: D,
) -> Result<Arguments, D::Error> {
	let s = String::deserialize(deserializer)?;
	if s.is_empty() {
		return Ok(Arguments::new());
	}
	serde_json::from_str(&s).map_err(de::Error::custom)
}

/// Tool call arguments
pub type Arguments = serde_json::Map<String, serde_json::Value>;
