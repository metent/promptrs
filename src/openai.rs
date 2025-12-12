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
use futures_lite::StreamExt;
#[cfg(target_os = "wasi")]
use http::header::{AUTHORIZATION, CONTENT_TYPE};
#[cfg(target_os = "wasi")]
use wstd::http::body::Bytes;
#[cfg(target_os = "wasi")]
use wstd::http::body::util::BodyDataStream;
#[cfg(target_os = "wasi")]
use wstd::http::body::util::combinators::UnsyncBoxBody;
#[cfg(target_os = "wasi")]
use wstd::http::{Body, BodyExt, Client, Request as Req};

/// Represents a chat completion request configuration.
pub struct Request<'s, S> {
	/// API key for authentication
	pub api_key: &'s Option<String>,
	/// Base URL to which /v1/chat/completions will be concatenated
	pub base_url: &'s String,
	/// JSON body
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
		let mut stream = Box::pin(self.stream(body));
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
	async fn body(&self) -> io::Result<Body> {
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
			.body(Body::from_json(&self.body)?)
			.map(|req| client.send(req))
			.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?
			.await
			.map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
		let status = response.status();
		if !status.is_success() {
			let mut body = response.into_body();
			let resp = body
				.contents()
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
	fn stream(&self, body: Body) -> ChatCompletionStream {
		ChatCompletionStream::new(body.into_boxed_body().into_data_stream())
	}
}

/// API request parameters
pub struct Params<'s, S> {
	/// Model identifier (e.g., "gpt-4", "claude-2")
	pub model: &'s String,
	/// Sampling temperature for response randomness (0-2)
	pub temperature: Option<f64>,
	/// Nucleus sampling parameter for diversity control (0-1)
	pub top_p: Option<f64>,
	/// Inner params
	pub inner: InnerParams<'s, S>,
	/// Stream mode flag
	pub stream: bool,
	/// Extra params
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

/// Inner params
pub struct InnerParams<'s, S> {
	/// List of messages
	pub messages: &'s [Message],
	/// List of tools
	pub tools: &'s [Tool<'s, S>],
	/// Status tool
	pub status: &'s Option<Tool<'s, S>>,
	/// System prompt mode
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
			let mut content = vec![Part::Text {
				text: system.clone(),
			}];
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
#[derive(Clone, Debug, Deserialize, Serialize)]
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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "type")]
/// Part of a message
pub enum Part {
	/// Text Part
	#[serde(rename = "text")]
	Text {
		/// Text Field
		text: String,
	},
	/// Image Part
	#[serde(rename = "image_url")]
	Image {
		/// Image URL Field
		image_url: ImageUrl,
	},
}

/// Image URl
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ImageUrl {
	/// URL field
	pub url: String,
}

/// Response structure containing AI output and tool calls.
#[derive(Serialize)]
pub struct Response {
	/// Optional assistant reasoning about response generation
	pub reasoning: Option<String>,
	/// Main response content from AI
	pub content: String,
	/// List of executed tool function calls
	pub tool_calls: Vec<Function>,
}

/// Response segment
#[derive(Debug, Serialize)]
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
/// Chat Completion Chunk
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
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Function {
	/// Name of the tool function to invoke
	pub name: String,
	/// Arguments for the tool in JSON format
	pub arguments: Arguments,
}

/// Tool call arguments
pub type Arguments = serde_json::Map<String, serde_json::Value>;

#[cfg(target_os = "wasi")]
struct ChatCompletionStream {
	buf: Vec<u8>,
	inner: BodyDataStream<UnsyncBoxBody<Bytes, wstd::http::Error>>,
}

#[cfg(target_os = "wasi")]
impl ChatCompletionStream {
	fn new(inner: BodyDataStream<UnsyncBoxBody<Bytes, wstd::http::Error>>) -> Self {
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

	pub async fn read_line(&mut self) -> io::Result<Option<Vec<u8>>> {
		loop {
			if let Some(nl) = self.buf.iter().position(|&c| c == b'\n') {
				let line = self.buf.drain(..nl + 1).collect::<Vec<_>>();
				return Ok(Some(line));
			}

			match self.inner.next().await {
				Some(Ok(bytes)) => {
					self.buf.extend_from_slice(&bytes);
				}
				Some(Err(e)) => return Err(io::Error::other(e)),
				None => {
					if self.buf.is_empty() {
						return Ok(None);
					}
					let line = self.buf.drain(..).collect::<Vec<_>>();
					return Ok(Some(line));
				}
			}
		}
	}
}
