#![deny(missing_docs)]

//! # `promptrs`
//!
//! This crate provides a framework for building agentic workflows that integrate language models
//! with tool calling capabilities. It handles conversation management, API interactions, tool
//! invocation, and response parsing in a streamlined, composable manner. The core design
//! philosophy is to enable fluent method chaining while maintaining minimal state management.
//! `promptrs` keeps everything synchronous, tiny, and highly composable, making it easy to drop
//! into any existing Rust binary or library.
//!
//! It allows you to write agents using a builder pattern using which it can:
//! * Keep a conversation history with soft‑pruning.
//! * Call user‑defined **tools** (Rust functions) from the assistant’s reply.
//! * Parse and format tool calls in three different styles:
//!   * OpenAI JSON‑schema / function calls (default when `paradigm: Server`)
//!   * Custom JSON‑schema wrapped in delimiters (`ToolCallParadigm::JsonSchema`)
//!   * A *Pythonic* style – the assistant emits a python function call in a code block.
//! * Add a **status** tool that can report any agent state after each user message.
//! * Stream the assistant’s reply in “real‑time” and optionally hand every token to a
//!   user supplied callback.
//! * Serialize a lot of configuration from a single struct that can be deserialized
//!   from JSON/YAML.
//!
//! ```rust
//! use promptrs::{ToolCallParadigm, UserConfig, tool};
//! use std::collections::HashMap;
//!
//! #[derive(Default)]
//! struct AppState {
//!     notes: HashMap<String, String>,
//! }
//!
//! #[tool]
//! fn add_note(state: &mut AppState, id: String, title: String) -> String {
//!     state.notes.insert(id.clone(), title);
//!     format!("✅ Note '{}' added", id)
//! }
//!
//! fn main() -> anyhow::Result<()> {
//!     let cfg = UserConfig::builder()
//!         .api_key(Some("sk-…".into()))
//!         .base_url("https://api.openai.com".into())
//!         .model("gpt-4o-mini".into())
//!         .paradigm(ToolCallParadigm::Pythonic)
//!         .char_limit(8192)
//!         .build();
//!
//!     let mut state = AppState::default();
//!     let reply = cfg
//!         .system(Some("Assistant helps with note taking"))
//!         .tool(add_note)
//!         .user("Add a note titled ‘Meeting’")
//!         .process(&mut state)?;
//!
//!     println!("{:?}", reply.text);
//! #   Ok(())
//! }
//! ```

mod client;
mod parser;
mod pruner;

pub use client::{Arguments, Function, Message, Segment};
use client::{InnerParams, Params, Request, Response};
use log::debug;
use parser::parse;
use pruner::prune;
pub use serde;
use serde::Deserialize;
pub use serde_json;
use std::io;
pub use tool_attr_macro::tool;

/// Configuration for LLM interactions.
///
/// The `UserConfig` is meant for long‑lived objects that you can store in a config file or
/// environment. Deserialize it with `serde_json`, `serde_yaml`, or build it manually through
/// the builder API.
///
/// # Example
///
/// ```rust
/// let cfg = UserConfig::builder()
///     .api_key(Some("sk-…".into()))
///     .base_url("https://api.openai.com".into())
///     .model("gpt-4o".into())
///     .build();
/// ```
#[derive(Default, Deserialize)]
pub struct UserConfig {
	/// API authentication key (if required by provider)
	pub api_key: Option<String>,
	/// Base URL for API endpoints
	pub base_url: String,
	/// Model identifier (e.g., "gpt-4", "claude-2")
	pub model: String,
	/// Sampling temperature for response randomness (0-2)
	pub temperature: Option<f64>,
	/// Nucleus sampling parameter for diversity control (0-1)
	pub top_p: Option<f64>,
	/// Delimiters for assistant content
	pub delims: Option<Delims>,
	/// Whether the system prompt is sent as a system message or as part of the first user message.
	pub mode: SystemPromptMode,
	/// Maximum allowed message history size in bytes
	pub char_limit: u64,
	/// Additional chat completion request parameters
	#[serde(default)]
	pub extra: serde_json::Map<String, serde_json::Value>,
}

impl UserConfig {
	/// Creates a new configuration builder with default values
	pub fn builder() -> UserConfigBuilder {
		UserConfigBuilder(UserConfig::default())
	}

	/// Start a workflow by adding a system prompt.  The returned `InitState` holds the
	/// configuration, the system message (if any) and an empty tool list.
	pub fn system<S>(&self, system: Option<String>) -> InitState<'_, '_, S> {
		InitState {
			config: self,
			system,
			tools: Vec::new(),
			status: None,
		}
	}
}

/// Convenience type for building an LLM configuration.
pub struct UserConfigBuilder(UserConfig);

impl UserConfigBuilder {
	/// Sets API authentication key
	pub fn api_key(mut self, api_key: Option<String>) -> Self {
		self.0.api_key = api_key;
		self
	}

	/// Configures temperature parameter for response generation
	pub fn temperature(mut self, temperature: Option<f64>) -> Self {
		self.0.temperature = temperature;
		self
	}

	/// Sets Nucleus sampling parameter
	pub fn top_p(mut self, top_p: Option<f64>) -> Self {
		self.0.top_p = top_p;
		self
	}

	/// Sets delimiters for assistant content
	pub fn delims(mut self, delims: Option<Delims>) -> Self {
		self.0.delims = delims;
		self
	}

	/// Sets maximum message history size
	pub fn char_limit(mut self, char_limit: u64) -> Self {
		self.0.char_limit = char_limit;
		self
	}

	/// Sets additional chat completion request parameters
	pub fn extra(mut self, extra: serde_json::Map<String, serde_json::Value>) -> Self {
		self.0.extra = extra;
		self
	}

	/// Finalizes configuration with API base url and model selection
	pub fn build(mut self, base_url: String, model: String) -> UserConfig {
		self.0.base_url = base_url;
		self.0.model = model;
		self.0
	}
}

/// Initial agent state containing system configuration
pub struct InitState<'c, 's, S> {
	config: &'c UserConfig,
	system: Option<String>,
	tools: Vec<Tool<'s, S>>,
	status: Option<Tool<'s, S>>,
}

impl<'c, 's, S> InitState<'c, 's, S> {
	/// Add a first user message and turn the builder into a *SendState*.
	///
	/// This moves the system prompt into the messages array and returns
	/// a new struct that is ready for extra messages and token callbacks.
	pub fn user(self, user: String) -> SendState<'c, 's, S> {
		let messages = self
			.system
			.into_iter()
			.map(Message::System)
			.chain([Message::User(user)])
			.collect();
		SendState {
			config: self.config,
			messages,
			tools: self.tools,
			status: self.status,
		}
	}

	/// Registers a new tool implementation
	pub fn tool(mut self, tool: Tool<'s, S>) -> Self {
		self.tools.push(tool);
		self
	}

	/// Register a *status* function that will be called once after the
	/// LLM has processed a user request.  The return value becomes a
	/// message of type `Message::Status`, which is later filtered out
	/// and persisted across turns.
	pub fn status(mut self, tool: Tool<'s, S>) -> Self {
		self.status = Some(tool);
		self
	}
}

/// State that actually sends the request.
///
/// Holds the conversation history and the tool set.  It can emit a callback
/// for every streaming token, execute the tools found in the assistant reply,
/// run the status tool, and return a `ReceivedState` that can be continued.
pub struct SendState<'c, 's, S> {
	config: &'c UserConfig,
	messages: Vec<Message>,
	tools: Vec<Tool<'s, S>>,
	status: Option<Tool<'s, S>>,
}

impl<'c, 's, S> SendState<'c, 's, S> {
	/// Add an arbitrary list of messages to the history.
	///
	/// Useful for bulk‑adding system, user or assistant messages.
	/// Adds additional messages to the history
	pub fn messages(mut self, messages: impl IntoIterator<Item = Message>) -> Self {
		self.messages.extend(messages);
		self
	}

	/// Register a callback that receives each token that the assistant streams.
	pub fn on_token<F: FnMut(&mut S, String, bool) -> String>(
		self,
		on_token: F,
	) -> SendAndHandleTokenState<'c, 's, S, F> {
		SendAndHandleTokenState {
			send_state: self,
			on_token,
		}
	}

	/// Dispatch the request and parse the response.
	///
	/// It performs a *synchronous* network request, parses the chunks,
	/// executes any tool calls and the status tool, and returns a
	/// structure that contains the assistant reply as `text` – ready
	/// for printing or sending back to the LLM.
	pub async fn process(self, state: &mut S) -> io::Result<ReceivedState<'c, 's, S>> {
		self.process_inner(state, None::<fn(&mut S, String, bool) -> String>)
			.await
	}

	async fn process_inner(
		mut self,
		state: &mut S,
		mut on_token: Option<impl FnMut(&mut S, String, bool) -> String>,
	) -> io::Result<ReceivedState<'c, 's, S>> {
		self.push_initial_status(state);

		debug!("Messages: {:#?}", self.messages);

		let completion = self
			.completion(
				on_token
					.as_mut()
					.map(|handler| |token, is_reasoning| handler(state, token, is_reasoning)),
			)
			.await?;
		let (assistant, segments) = self.parse_response(completion);
		if let Some(assistant) = &assistant {
			self.messages.push(Message::Assistant(assistant.clone()));
		}

		let old_len = self.messages.len();
		let empty = vec![];
		let tool_calls = segments.iter().flat_map(|seg| match seg {
			Segment::ToolCall(tc) => tc.iter(),
			_ => empty.iter(),
		});
		let called = self.execute_tools(state, tool_calls);
		if called {
			self.push_status(state);
		}
		let nbtc = self.messages.len() - old_len;

		Ok(ReceivedState {
			config: self.config,
			messages: self.messages,
			tools: self.tools,
			nbtc,
			status: self.status,
			text: assistant,
			segments,
		})
	}

	async fn completion(
		&self,
		on_token: Option<impl FnMut(String, bool) -> String>,
	) -> io::Result<Response> {
		Request {
			api_key: &self.config.api_key,
			base_url: &self.config.base_url,
			body: Params {
				model: &self.config.model,
				temperature: self.config.temperature,
				top_p: self.config.top_p,
				inner: InnerParams {
					messages: &self.messages,
					tools: &self.tools,
					status: &self.status,
					mode: self.config.mode,
				},
				stream: true,
				extra: &self.config.extra,
			},
		}
		.chat_completion(on_token)
		.await
	}

	fn parse_response(&self, mut response: Response) -> (Option<String>, Vec<Segment>) {
		let parsed = parse(&mut response.content.as_str(), self.config.delims.as_ref())
			.unwrap_or_else(|_| vec![Segment::Commentary(response.content.clone())])
			.into_iter()
			.filter({
				let parse_reasoning = response.reasoning.is_none();
				let parse_tool_calls = response.tool_calls.is_empty();
				move |seg| match seg {
					Segment::Reasoning(_) => parse_reasoning,
					Segment::ToolCall(_) => parse_tool_calls,
					_ => true,
				}
			});

		let mut segments = Vec::new();
		if let Some(reasoning) = response.reasoning {
			segments.push(Segment::Reasoning(reasoning));
		} else if let Some(delims) = &self.config.delims {
			if let Some(start) = response.content.find(&delims.reasoning.1) {
				response.content = response.content.split_off(start + delims.reasoning.1.len());
			}
		}
		segments.extend(parsed);
		segments.push(Segment::ToolCall(response.tool_calls));

		debug!("Segments: {segments:#?}");

		if !response.content.trim().is_empty() {
			(Some(response.content), segments)
		} else {
			(None, segments)
		}
	}

	fn execute_tools<'f>(
		&mut self,
		state: &mut S,
		tool_calls: impl Iterator<Item = &'f Function>,
	) -> bool {
		let mut called = false;
		for func in tool_calls {
			if self.status.as_ref().is_some_and(|s| s.name == func.name) {
				continue;
			}
			let mut arguments = func.arguments.clone();
			let resp = self
				.tools
				.iter()
				.find(|tool| tool.name == func.name)
				.map(|tool| (tool.call)(state, tool.name, &mut arguments))
				.unwrap_or_else(|| Ok("Function not found.".into()))
				.unwrap_or_else(|err| err.to_string());

			self.messages.push(Message::ToolCall((
				Function {
					name: func.name.clone(),
					arguments,
				},
				resp,
			)));
			called = true;
		}
		called
	}

	fn push_initial_status(&mut self, state: &mut S) {
		let mut j = None;
		let mut has_status = false;

		for (i, message) in self.messages.iter().enumerate() {
			match message {
				Message::User(_) if j.is_none() => j = Some(i),
				Message::Status(_) => has_status = true,
				_ => {}
			}
		}

		let (Some(i), false, Some(status_fn)) = (j, has_status, &self.status) else {
			return;
		};

		let status = (status_fn.call)(state, status_fn.name, &mut serde_json::Map::new())
			.unwrap_or("Unexpected error while serializing status!".into());

		self.messages.insert(
			i + 1,
			Message::Status((
				Function {
					name: status_fn.name.into(),
					arguments: Arguments::new(),
				},
				status,
			)),
		);
	}

	fn push_status(&mut self, state: &mut S) {
		let Some(status_fn) = &self.status else {
			return;
		};
		let status = (status_fn.call)(state, status_fn.name, &mut serde_json::Map::new())
			.unwrap_or("Unexpected error while serializing status!".into());
		self.messages.push(Message::Status((
			Function {
				name: status_fn.name.into(),
				arguments: Arguments::new(),
			},
			status,
		)));
	}
}

/// Wrapper around a `SendState` + a token callback.
pub struct SendAndHandleTokenState<'c, 's, S, F: FnMut(&mut S, String, bool) -> String> {
	send_state: SendState<'c, 's, S>,
	on_token: F,
}

impl<'c, 's, S, F: FnMut(&mut S, String, bool) -> String> SendAndHandleTokenState<'c, 's, S, F> {
	/// Adds additional messages to the history
	pub fn messages(mut self, messages: impl IntoIterator<Item = Message>) -> Self {
		self.send_state.messages.extend(messages);
		self
	}

	/// Starts processing with token callback
	pub async fn process(self, state: &mut S) -> io::Result<ReceivedState<'c, 's, S>> {
		self.send_state
			.process_inner(state, Some(self.on_token))
			.await
	}
}

/// Result of processing a user request.
///
/// Contains the raw text (without the reasoning wrapper) and the updated history.
pub struct ReceivedState<'c, 's, S> {
	config: &'c UserConfig,
	messages: Vec<Message>,
	tools: Vec<Tool<'s, S>>,
	nbtc: usize,
	status: Option<Tool<'s, S>>,
	/// Assistant response with reasoning block removed
	pub text: Option<String>,
	/// Parsed segments of assistant response
	pub segments: Vec<Segment>,
}

impl<'c, 's, T> ReceivedState<'c, 's, T> {
	/// Continues conversation with new user input
	pub fn user(mut self, user: Option<String>) -> SendState<'c, 's, T> {
		self.messages.extend(user.into_iter().map(Message::User));
		self.messages = prune(self.messages, self.config.char_limit);
		SendState {
			config: self.config,
			messages: self.messages,
			tools: self.tools,
			status: self.status,
		}
	}

	/// Return a slice of tool‑call messages that were invoked during the latest iteration.
	pub fn tool_calls(&self) -> &[Message] {
		&self.messages[self.messages.len() - self.nbtc..]
	}
}

/// System prompt mode
#[derive(Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SystemPromptMode {
	/// Send system prompt in system message
	#[default]
	System,
	/// Send system prompt inside user message
	User,
}

/// Delimiter configuration for structured message parsing.
#[derive(Default, Deserialize)]
pub struct Delims {
	/// Delimiters for assistant reasoning
	pub reasoning: (String, String),
	/// Optional delimiters for assistant answer
	pub content: Option<(String, String)>,
}

/// Tool metadata
#[derive(Clone, Copy)]
pub struct Tool<'s, S> {
	/// Tool name
	pub name: &'s str,
	/// Tool description
	pub description: &'s str,
	/// Tool parameters in JSON schema format
	pub jsonschema: &'s str,
	/// Pointer to tool implementation
	pub call:
		fn(state: &mut S, name: &str, arguments: &mut Arguments) -> serde_json::Result<String>,
}
