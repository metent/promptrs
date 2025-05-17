use attohttpc::body::Json;
use attohttpc::header::CONTENT_TYPE;
use attohttpc::{ResponseReader, StatusCode, TextReader};
use either::IntoEither;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::io::{BufRead, BufReader, Lines};
use std::iter::StepBy;
use std::time::Duration;

#[derive(thiserror::Error, Debug)]
pub enum OpenAIError {
	#[error("Network error: {0}")]
	NetworkError(#[from] attohttpc::Error),
	#[error("API error with status code {0}: {1}")]
	APIError(StatusCode, String),
	#[error("Chunk error")]
	Chunk,
	#[error("Deserialization error")]
	DeserError(#[from] serde_json::Error),
}

#[derive(Serialize)]
pub struct ChatCompletionRequest<'s> {
	model: &'s str,
	messages: &'s [Message<'s>],
	#[serde(skip_serializing_if = "Option::is_none")]
	tools: Option<&'s [Value]>,
	stream: bool,
}

#[derive(Debug, Serialize, Clone)]
#[serde(tag = "role", rename_all = "snake_case")]
pub enum Message<'s> {
	System {
		content: &'s str,
	},
	User {
		content: &'s str,
	},
	Assistant {
		content: String,
	},
	Tool {
		content: &'s str,
		tool_call_id: &'s str,
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

pub struct ChatReader(ResponseReader);

impl ChatReader {
	pub fn new(
		model: &str,
		messages: &[Message<'_>],
		base_url: &str,
		api_token: Option<&str>,
		openai_tools: Option<&[Value]>,
	) -> Result<ChatReader, OpenAIError> {
		let body = ChatCompletionRequest {
			model,
			messages,
			tools: openai_tools,
			stream: true,
		};

		let (status, _, reader) = attohttpc::post(base_url.to_string() + "/v1/chat/completions")
			.read_timeout(Duration::from_secs(600))
			.into_either(api_token.is_some())
			.right_or_else(|req| req.bearer_auth(api_token.as_slice()[0]))
			.header(CONTENT_TYPE, "application/json")
			.body(Json(body))
			.send()?
			.split();
		if !status.is_success() {
			let resp = reader.text()?;
			return Err(OpenAIError::APIError(status, resp));
		}

		Ok(ChatReader(reader))
	}

	pub fn stream(self) -> ChatCompletionStream {
		ChatCompletionStream(
			BufReader::new(TextReader::new(self.0, attohttpc::charsets::UTF_8))
				.lines()
				.step_by(2),
		)
	}
}

pub struct ChatCompletionStream(StepBy<Lines<BufReader<TextReader<ResponseReader>>>>);

impl Iterator for ChatCompletionStream {
	type Item = Result<ChatCompletionChunk, OpenAIError>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.filter(|l| !l.as_ref().is_ok_and(|l| l == "data: [DONE]"))?
			.ok()
			.filter(|l| l.len() > 6)
			.map(|l| Ok(serde_json::from_str(&l[6..])?))
			.or(Err(OpenAIError::Chunk).into())
	}
}
