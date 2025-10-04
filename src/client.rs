//! Core client implementation for chat completions.
use super::{SystemPromptMode, Tool};
use either::IntoEither;
#[cfg(not(target_os = "wasi"))]
use futures_util::{AsyncBufReadExt, Stream, StreamExt, TryStreamExt};
use log::info;
#[cfg(not(target_os = "wasi"))]
use reqwest::Client;
#[cfg(not(target_os = "wasi"))]
use reqwest::header::CONTENT_TYPE;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Deserialize, Serialize, Serializer};
use serde_json::json;
use serde_json::value::RawValue;
use std::collections::HashMap;
use std::io;
#[cfg(not(target_os = "wasi"))]
use std::time::Duration;

#[cfg(target_os = "wasi")]
use http::header::{AUTHORIZATION, CONTENT_TYPE};
#[cfg(target_os = "wasi")]
use wstd::http::body::IncomingBody;
#[cfg(target_os = "wasi")]
use wstd::http::request::JsonRequest;
#[cfg(target_os = "wasi")]
use wstd::http::{Client, Request as Req};
#[cfg(target_os = "wasi")]
use wstd::io::{AsyncInputStream, AsyncRead as _};

/// Represents a chat completion request configuration.
pub struct Request<'s, S> {
	pub api_key: &'s Option<String>,
	pub base_url: &'s String,
	pub body: Params<'s, S>,
}

impl<'s, S> Request<'s, S> {
	/// Sends the chat completion request and processes streaming response.
	pub async fn chat_completion(
		&self,
		on_token: &mut Option<impl FnMut(String, bool) -> String>,
	) -> io::Result<Response> {
		let mut buf = String::new();
		let mut rbuf = String::new();
		let mut tool_calls = Vec::new();
		let mut content = String::new();
		let mut reasoning = String::new();
		#[cfg(not(target_os = "wasi"))]
		let mut stream = Box::pin(self.stream().await?);
		#[cfg(target_os = "wasi")]
		let body = self.body().await?;
		#[cfg(target_os = "wasi")]
		let mut stream = Box::pin(self.stream(&body));
		while let Some(chunk) = stream.next().await {
			let Ok(chunk) = chunk else {
				continue;
			};

			let Some(Choice {
				index,
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
				for ToolCall { function } in tcs {
					if index + 1 > tool_calls.len() {
						tool_calls.resize(index + 1, RawFunction::default());
					}

					let name = tool_calls[index].name.take().unwrap_or_default();
					tool_calls[index].name =
						Some(name + function.name.as_ref().map_or("", |name| name.as_str()));

					let arguments = tool_calls[index].arguments.take().unwrap_or_default();
					tool_calls[index].arguments = Some(
						arguments + function.arguments.as_ref().map_or("", |args| args.as_str()),
					);
				}
			}

			if let Some(mut text) = cnt {
				if let Some(f) = on_token.as_mut() {
					text = f(text, false);
				}

				buf += &text;
				while let Some(end) = buf.find('\n') {
					info!("{}", &buf[..end]);
					buf = buf.split_off(end + 1);
				}

				content.push_str(text.as_str());
			}

			if let Some(mut text) = reasoning_content {
				if let Some(f) = on_token.as_mut() {
					text = f(text, true);
				}

				rbuf += &text;
				while let Some(end) = rbuf.find('\n') {
					info!("{}", &rbuf[..end]);
					rbuf = rbuf.split_off(end + 1);
				}

				reasoning.push_str(text.as_str());
			}
		}

		Ok(Response {
			reasoning: reasoning.len().gt(&0).then_some(reasoning),
			content,
			tool_calls: tool_calls
				.into_iter()
				.filter_map(|tc| {
					Some(Function {
						name: tc.name?,
						arguments: serde_json::from_str(&tc.arguments?).ok()?,
					})
				})
				.collect(),
		})
	}

	#[cfg(not(target_os = "wasi"))]
	async fn stream(&self) -> io::Result<impl Stream<Item = io::Result<ChatCompletionChunk>>> {
		let response = Client::new()
			.post(self.base_url.to_string() + "/v1/chat/completions")
			.into_either(self.api_key.is_some())
			.right_or_else(|req| req.bearer_auth(self.api_key.as_ref().as_slice()[0]))
			.header(CONTENT_TYPE, "application/json")
			.json(&self.body)
			.timeout(Duration::MAX)
			.send()
			.await
			.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
		let status = response.status();
		if !status.is_success() {
			let resp = response
				.text()
				.await
				.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
			return Err(io::Error::from(io::Error::new(
				io::ErrorKind::ConnectionRefused,
				format!("Status Code: {}\nResponse: {resp}", status),
			)));
		}
		let stream = response
			.bytes_stream()
			.map(|x| x.map_err(|err| io::Error::new(io::ErrorKind::Other, err)))
			.into_async_read()
			.lines()
			.filter_map(async |l| {
				let s = l.ok()?;
				(!s.contains("[DONE]")).then_some(())?;
				let i = s.find('{')?;
				Some(serde_json::from_str(&s[i..]).map_err(|err| {
					io::Error::new(io::ErrorKind::InvalidData, format!("Malformed JSON: {err}"))
				}))
			});

		Ok(stream)
	}

	#[cfg(target_os = "wasi")]
	async fn body(&self) -> io::Result<IncomingBody> {
		let client = Client::new();
		let response = Req::post(self.base_url.to_string() + "/v1/chat/completions")
			.into_either(self.api_key.is_some())
			.right_or_else(|req| {
				req.header(
					AUTHORIZATION,
					format!("Bearer {}", self.api_key.as_ref().as_slice()[0]),
				)
			})
			.header(CONTENT_TYPE, "application/json")
			.json(&self.body)
			.map(|req| client.send(req))
			.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?
			.await
			.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
		let status = response.status();
		if !status.is_success() {
			let resp = response
				.into_body()
				.bytes()
				.await
				.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
			return Err(io::Error::from(io::Error::new(
				io::ErrorKind::ConnectionRefused,
				format!(
					"Status Code: {}\nResponse: {}",
					std::str::from_utf8(&resp).unwrap_or(""),
					status
				),
			)));
		}

		Ok(response.into_body())
	}

	#[cfg(target_os = "wasi")]
	fn stream<'b>(&self, body: &'b IncomingBody) -> ChatCompletionStream<'b> {
		ChatCompletionStream::new(body.as_async_input_stream().unwrap())
	}
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
		if self.temperature.is_some() {
			map.serialize_entry("temperature", &self.temperature)?;
		}
		if self.top_p.is_some() {
			map.serialize_entry("top_p", &self.top_p)?;
		}
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

		#[derive(Serialize)]
		#[serde(tag = "role", rename = "user")]
		struct User<'s> {
			content: &'s [Part],
		}

		if let (Some(Message::System(system)), SystemPromptMode::User) = (curr, mode) {
			let mut content = vec![Part::Text(system.clone())];
			if let Some(Message::User(user)) = curr {
				content.extend(user.iter().cloned());
			}
			seq.serialize_element(&User { content: &content })?;
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

			match message {
				Message::System(content) => {
					seq.serialize_element(&HashMap::from([
						("role", "system"),
						("content", &content),
					]))?;
				}
				Message::User(content) => seq.serialize_element(&User { content: &content })?,
				Message::Assistant(content) => {
					seq.serialize_element(&HashMap::from([
						("role", "assistant"),
						("content", &content),
					]))?;
				}
				_ => unreachable!(),
			}

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
	User(Vec<Part>),
	/// LLM responses without reasoning
	Assistant(String),
	/// Tool call identifier, arguments and response
	ToolCall((Function, String)),
	/// Status tool call identifer and response
	Status((Function, String)),
}

#[derive(Clone, Debug)]
/// Represents part of a message
pub enum Part {
	/// Text Content
	Text(String),
	/// Image Url
	Image(String),
}

impl Serialize for Part {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let mut map = serializer.serialize_map(Some(2))?;
		match self {
			Part::Text(text) => {
				map.serialize_entry("type", "text")?;
				map.serialize_entry("text", text)?;
			}
			Part::Image(url) => {
				map.serialize_entry("type", "image_url")?;
				map.serialize_entry("image_url", &HashMap::from([("url", url)]))?;
			}
		}
		map.end()
	}
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
#[derive(Debug)]
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
	#[serde(default)]
	index: usize,
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
	function: RawFunction,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct RawFunction {
	name: Option<String>,
	arguments: Option<String>,
}

/// Represents a function call to an external tool.
#[derive(Clone, Debug)]
pub struct Function {
	/// Name of the tool function to invoke
	pub name: String,
	/// Arguments for the tool in JSON format
	pub arguments: Arguments,
}

/// Tool call arguments
pub type Arguments = serde_json::Map<String, serde_json::Value>;

#[cfg(target_os = "wasi")]
struct ChatCompletionStream<'b> {
	buf: Vec<u8>,
	inner: &'b AsyncInputStream,
}

#[cfg(target_os = "wasi")]
impl<'b> ChatCompletionStream<'b> {
	fn new(inner: &'b AsyncInputStream) -> Self {
		ChatCompletionStream {
			buf: Vec::new(),
			inner,
		}
	}

	async fn next(&mut self) -> Option<io::Result<ChatCompletionChunk>> {
		self.read_chunk().await.transpose()
	}

	async fn read_chunk(&mut self) -> io::Result<Option<ChatCompletionChunk>> {
		while let Some(line) = self.read_line().await? {
			let line = match line.iter().rposition(|&c| c == b'\n' || c == b'\r') {
				Some(pos) => &line[..pos],
				None => &line[..],
			};

			let Ok(s) = std::str::from_utf8(line) else {
				continue;
			};
			if s.starts_with("[DONE]") {
				return Ok(None);
			}
			let Some(json_start) = s.find('{') else {
				continue;
			};

			let chunk_res = serde_json::from_str(&s[json_start..]).map_err(|e| {
				io::Error::new(io::ErrorKind::InvalidData, format!("Malformed JSON: {}", e))
			});

			return Some(chunk_res).transpose();
		}
		Ok(None)
	}

	async fn read_line(&mut self) -> io::Result<Option<Vec<u8>>> {
		loop {
			if let Some(nl) = self.buf.iter().position(|&c| c == b'\n') {
				let line = self.buf.drain(..nl + 1).collect::<Vec<_>>();
				return Ok(Some(line));
			}

			let mut chunk = [0u8; 1_024];
			let n = self.inner.read(&mut chunk).await?;
			if n == 0 {
				if self.buf.is_empty() {
					return Ok(None);
				}
				let line = self.buf.drain(..).collect::<Vec<_>>();
				return Ok(Some(line));
			}
			self.buf.extend_from_slice(&chunk[..n]);
		}
	}
}
