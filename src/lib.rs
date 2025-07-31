#![deny(missing_docs)]

//! # promptrs
//!
//! This crate provides a framework for building agentic workflows that integrate language models
//! with tool calling capabilities. It handles conversation management, API interactions, tool
//! invocation, and response parsing in a streamlined, composable manner. The core design
//! philosophy is to enable fluent method chaining while maintaining minimal state management.
//!
//! ## Features
//!
//! - Streamed API communication with incremental message processing
//! - Multi-tool support with function call validation and error handling
//! - Configurable delimiters for custom message formatting
//! - Automated message pruning for context size management
//! - JSON schema generation for tool documentation
//! - Pythonic tool calling support
//!
//! ## Usage Overview
//!
//! 1. **Configure API**: Set up model parameters and credentials
//! 2. **Define Tools**: Create functions with `#[tool]` attribute
//! 3. **Build Workflow**: Chain configuration and tool registration
//! 4. **Process Messages**: Handle user input through fluent API
//! 5. **Handle Responses**: Manage tool calls and state updates
//!
//! ## Example: Basic Workflow
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
//! /// Tool implementation for note-taking
//! /// id: Unique ID for note
//! /// title: Note title
//! #[tool]
//! fn add_note(state: &mut AppState, id: String, title: String) -> String {
//!     // Actual implementation would store the note
//!     format!("Note {id} added with title: {title}")
//! }
//!
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Configure language model
//!     let config = UserConfig::builder()
//!         .api_key(Some("sk-examplekey".to_string()))
//!         .temperature(Some(0.3))
//!         .paradigm(ToolCallParadigm::Pythonic)
//!         .char_limit(8192)
//!         .build("https://api.openai.com".to_string(), "gpt-4".to_string());
//!
//!     // Initialize conversation state
//!     let mut state = AppState::default();
//!
//!     // Setup conversation with system prompt and tools
//!     let init = config
//!         .system(Some(
//!             "You are an assistant that helps organize notes. Use add_note for new notes.",
//!         ))
//!         .tool(add_note);
//!
//!     // Process user interaction
//!     let response = init
//!         .user("Create a note about meeting agenda".to_string())
//!         .process(&mut state)?;
//!
//!     println!("Assistant response: {}", response.text);
//!
//!     // Continue conversation
//!     let next = response
//!         .user("Add a note for 'Creating promptrs'".to_string())
//!         .process(&mut state)?;
//!
//!     println!("Follow-up response: {}", next.text);
//!
//!     Ok(())
//! }
//! ```

mod client;
mod parser;
mod pruner;

pub use client::{Arguments, Message};
use client::{Function, Params, Request, Response};
use log::debug;
use parser::{parse, parse_py};
use pruner::prune;
pub use serde;
use serde::Deserialize;
pub use serde_json;
use serde_json::json;
use std::io;
pub use tool_attr_macro::tool;

/// User configuration for API interactions
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
	/// Tool calling configuration paradigm
	#[serde(flatten)]
	pub paradigm: ToolCallParadigm,
	/// Maximum allowed message history length in bytes
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
}

/// Builder for constructing a `UserConfig` with fluent API
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

	/// Configures tool invocation paradigm
	pub fn paradigm(mut self, paradigm: ToolCallParadigm) -> Self {
		self.0.paradigm = paradigm;
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

impl UserConfig {
	/// Initializes workflow configuration with system message
	pub fn system<'c, 's, S>(&'c self, system: Option<&'s str>) -> InitState<'c, 's, S> {
		InitState {
			config: self,
			system,
			tools: Vec::new(),
			status: None,
		}
	}
}

/// Initial agent state containing system configuration
pub struct InitState<'c, 's, S> {
	config: &'c UserConfig,
	system: Option<&'s str>,
	tools: Vec<Box<dyn Tool<State = S>>>,
	status: Option<Box<dyn Tool<State = S>>>,
}

impl<'c, S> InitState<'c, '_, S> {
	/// Starts user message chain
	pub fn user(self, user: String) -> SendState<'c, S> {
		let messages = self
			.system()
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
	pub fn tool(mut self, tool: impl Tool<State = S> + 'static) -> Self {
		self.tools.push(Box::new(tool));
		self
	}

	/// Sets status monitoring tool
	pub fn status(mut self, tool: impl Tool<State = S> + 'static) -> Self {
		self.status = Some(Box::new(tool));
		self
	}

	/// Generates system message with tool documentation
	fn system(&self) -> Option<String> {
		let system_base = self.system?;
		match &self.config.paradigm {
			ToolCallParadigm::JsonSchema(ToolDelims {
				available_tools,
				tool_call,
				..
			}) if !self.tools.is_empty() => Some(format!(
				r#"{system_base}
# Tools

You may call one or more functions to assist with the user query.

You are provided with function signatures within {at_start} and {at_end}:
{at_start}{oai_spec}{at_end}

For each function call, return a json object with function name and arguments within {tc_start} and {tc_end}:
{tc_start}{{"name": <function-name>, "arguments": <args-json-object>}}{tc_end}
"#,
				at_start = available_tools.0,
				at_end = available_tools.1,
				tc_start = tool_call.0,
				tc_end = tool_call.1,
				oai_spec = self.jsonschema()
			)),
			ToolCallParadigm::Pythonic if !self.tools.is_empty() => Some(format!(
				r#"{system_base}
# Tools

You may call one or more functions to assist with the user query. You are provided with the following function signatures:
```py
{pydefs}
```
You can invoke any of these functions by writing a python function call inside a python code block.
"#,
				pydefs = self.pydefs()
			)),
			_ => Some(system_base.into()),
		}
	}

	fn jsonschema(&self) -> String {
		let mut schemas = Vec::new();
		for tool in self.tools.iter() {
			schemas.push(json!({
				"name": tool.name(),
				"arguments": tool.arguments(),
			}));
		}
		serde_json::to_string_pretty(&schemas).unwrap_or_else(|err| err.to_string())
	}

	fn pydefs(&self) -> String {
		self.tools
			.iter()
			.fold("".into(), |acc, tool| acc + tool.pydef() + "\n")
	}
}

/// Active processing state containing message history
pub struct SendState<'c, S> {
	config: &'c UserConfig,
	messages: Vec<Message>,
	tools: Vec<Box<dyn Tool<State = S>>>,
	status: Option<Box<dyn Tool<State = S>>>,
}

impl<'c, S> SendState<'c, S> {
	/// Adds additional messages to the history
	pub fn messages(mut self, messages: impl IntoIterator<Item = Message>) -> Self {
		self.messages.extend(messages);
		self
	}

	/// Attaches token processing callback
	pub fn on_token<F: FnMut(&mut S, String)>(
		self,
		on_token: F,
	) -> SendAndHandleTokenState<'c, S, F> {
		SendAndHandleTokenState {
			send_state: self,
			on_token,
		}
	}

	/// Sends chat completion request, and handles parsing and tool call invocations
	pub fn process(self, state: &mut S) -> Result<ReceivedState<'c, S>, io::Error> {
		self.process_inner(state, None::<fn(&mut S, String)>)
	}

	/// Invoke chat completion API
	/// Parse assistant message and tool calls from raw assistant response text
	/// Invoke all tool calls
	/// Invoke a special tool that tracks completion status
	fn process_inner(
		mut self,
		state: &mut S,
		mut on_token: Option<impl FnMut(&mut S, String)>,
	) -> Result<ReceivedState<'c, S>, io::Error> {
		self.push_initial_status(state);

		debug!("Messages: {:#?}", self.messages);

		let completion = self.completion(
			on_token
				.as_mut()
				.map(|handler| |token| handler(state, token)),
		)?;
		let response = self.parse(completion);

		if !response.content.is_empty() {
			self.messages
				.push(Message::Assistant(response.content.clone()));
		}
		let called = self.execute_tools(state, response.tool_calls);
		if called {
			self.push_status(state);
		}

		Ok(ReceivedState {
			config: self.config,
			messages: self.messages,
			tools: self.tools,
			status: self.status,
			text: response.content,
		})
	}

	fn completion(&self, on_token: Option<impl FnMut(String)>) -> Result<Response, std::io::Error> {
		Ok(Request {
			api_key: &self.config.api_key,
			base_url: &self.config.base_url,
			body: Params {
				model: &self.config.model,
				temperature: self.config.temperature,
				top_p: self.config.top_p,
				messages: &self.messages,
				stream: true,
				extra: &self.config.extra,
			},
		}
		.chat_completion(on_token)?)
	}

	fn parse(&self, response: Response) -> Response {
		let parsed = match &self.config.paradigm {
			ToolCallParadigm::JsonSchema(delims) => parse(
				&mut response.content.as_str(),
				&self.config.delims,
				Some(delims),
			),
			ToolCallParadigm::Pythonic => {
				parse_py(&mut response.content.as_str(), &self.config.delims)
			}
			ToolCallParadigm::None => {
				parse(&mut response.content.as_str(), &self.config.delims, None)
			}
		}
		.unwrap_or(Response {
			reasoning: None,
			content: response.content,
			tool_calls: vec![],
		});

		let mut tool_calls = response.tool_calls;
		if tool_calls.is_empty() {
			tool_calls = parsed
				.tool_calls
				.into_iter()
				.map(|tc| Function {
					name: tc.name,
					arguments: tc.arguments,
				})
				.collect();
		}

		let text = parsed.content.trim();
		Response {
			reasoning: parsed.reasoning,
			content: text.into(),
			tool_calls,
		}
	}

	fn execute_tools(&mut self, state: &mut S, tool_calls: Vec<Function>) -> bool {
		let mut called = false;
		for tool_call in tool_calls {
			if self
				.status
				.as_ref()
				.is_some_and(|s| s.name() == tool_call.name)
			{
				continue;
			}
			let tc = match self.config.paradigm {
				ToolCallParadigm::JsonSchema { .. } => serde_json::to_string(&json!({
					"name": tool_call.name,
					"arguments": tool_call.arguments,
				}))
				.unwrap_or_default(),
				ToolCallParadigm::Pythonic => {
					format_python_call(&tool_call.name, &tool_call.arguments)
				}
				ToolCallParadigm::None => "".into(),
			};
			let resp = self
				.tools
				.iter()
				.find(|tool| tool.name() == tool_call.name)
				.map(|tool| tool.call(state, &tool_call.arguments))
				.unwrap_or_else(|| Ok("Function not found.".into()))
				.unwrap_or_else(|err| err.to_string());

			self.messages.push(Message::ToolCall((tc, resp)));
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

		let status = status_fn
			.call(state, &serde_json::Map::new())
			.unwrap_or("Unexpected error while serializing status!".into());

		self.messages
			.insert(i + 1, Message::Status((status_fn.name().into(), status)));
	}

	fn push_status(&mut self, state: &mut S) {
		let Some(status_fn) = &self.status else {
			return;
		};
		let status = status_fn
			.call(state, &serde_json::Map::new())
			.unwrap_or("Unexpected error while serializing status!".into());
		self.messages
			.push(Message::Status((status_fn.name().into(), status)));
	}
}

/// Processing state with token handling callback
pub struct SendAndHandleTokenState<'c, S, F: FnMut(&mut S, String)> {
	send_state: SendState<'c, S>,
	on_token: F,
}

impl<'c, S, F: FnMut(&mut S, String)> SendAndHandleTokenState<'c, S, F> {
	/// Adds additional messages to the history
	pub fn messages(mut self, messages: impl IntoIterator<Item = Message>) -> Self {
		self.send_state.messages.extend(messages);
		self
	}

	/// Starts processing with token callback
	pub fn process(self, state: &mut S) -> Result<ReceivedState<'c, S>, io::Error> {
		self.send_state.process_inner(state, Some(self.on_token))
	}
}

/// Result state after processing
pub struct ReceivedState<'c, S> {
	config: &'c UserConfig,
	messages: Vec<Message>,
	tools: Vec<Box<dyn Tool<State = S>>>,
	status: Option<Box<dyn Tool<State = S>>>,
	/// Assistant response with reasoning block removed
	pub text: String,
}

impl<'c, T> ReceivedState<'c, T> {
	/// Continues conversation with new user input
	pub fn user(mut self, user: String) -> SendState<'c, T> {
		self.messages.push(Message::User(user));
		self.messages = prune(self.messages, self.config.char_limit);
		SendState {
			config: self.config,
			messages: self.messages,
			tools: self.tools,
			status: self.status,
		}
	}
}

/// Tool calling paradigm configuration
#[derive(Default, Deserialize)]
#[serde(tag = "paradigm", rename_all = "snake_case")]
pub enum ToolCallParadigm {
	/// Standard, JSONschema based tool calling
	JsonSchema(ToolDelims),
	/// Pythonic tool calling
	Pythonic,
	/// No tool calling
	#[default]
	None,
}

/// Delimiter configuration for structured message parsing.
#[derive(Default, Deserialize)]
pub struct Delims {
	/// Delimiters for assistant reasoning
	pub reasoning: (String, String),
	/// Optional delimiters for assistant answer
	pub content: Option<(String, String)>,
}

/// Delimiter configuration for parsing and formatting tool calls.
#[derive(Default, Deserialize)]
pub struct ToolDelims {
	/// Delimiters for available tools section
	pub available_tools: (String, String),
	/// Delimiters for tool call blocks
	pub tool_call: (String, String),
	/// Delimiters for tool response blocks
	pub tool_response: (String, String),
}

/// Interface for tool implementations.
pub trait Tool {
	/// Agent State
	type State;

	/// Retrieves tool name
	fn name(&self) -> &str;

	/// Retrieves argument schema description
	fn arguments(&self) -> &str;

	/// Retrieves function interface for pythonic tool calling
	fn pydef(&self) -> &str;

	/// Executes the tool function with given JSON arguments
	fn call(&self, state: &mut Self::State, arguments: &Arguments) -> serde_json::Result<String>;
}

fn format_python_call(
	name: &str,
	arguments: &serde_json::Map<String, serde_json::Value>,
) -> String {
	let mut args = arguments
		.iter()
		.map(|(name, value)| format!("{name}={value}, "))
		.collect::<String>();
	if args.ends_with(", ") {
		args.truncate(args.len() - 2);
	}
	format!("{name}({args})")
}
