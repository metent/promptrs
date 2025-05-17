use crate::openai::{ChatReader, OpenAIError};
use crate::prompt::Prompter;
use env_logger::Target;
use log::{info, warn};
use std::io;
use std::thread::sleep;
use std::time::Duration;

pub struct Agent {
	prompter: Prompter,
	dir: String,
	allowed: Vec<String>,
}

impl Agent {
	pub fn new(name: String, dir: String, allowed: Vec<String>) -> io::Result<Self> {
		env_logger::builder()
			.target(Target::Stderr)
			.filter_level(log::LevelFilter::Info)
			.init();

		Ok(Self {
			prompter: Prompter::new(name),
			dir,
			allowed,
		})
	}

	pub fn process(mut self) {
		loop {
			self.prompter = match self.prompter.run(&self.dir, &self.allowed) {
				Ok(mut res) => {
					info!("\n\nGenerated prompt: {:?}", res);
					_ = res.from_agent.insert(self.prompter.to_agent);
					res
				}
				Err(err) => {
					warn!("\n\nPrompt Generation Failed: {}", err);
					self.prompter
				}
			};

			let raw_response = match self.chat_completion() {
				Ok(res) => res,
				Err(err) => {
					warn!("\n\nChat Completion Failed: {}", err);
					sleep(Duration::from_secs(10));
					continue;
				}
			};

			_ = self.prompter.response.insert(raw_response);
		}
	}

	fn chat_completion(&self) -> Result<String, OpenAIError> {
		let reader = ChatReader::new(
			&self.prompter.model.as_ref().map_or("", |m| m.as_str()),
			&self.prompter.messages(),
			self.prompter
				.base_url
				.as_ref()
				.map(|u| u.as_str())
				.unwrap_or(""),
			self.prompter.api_token.as_ref().map(|t| t.as_str()),
			self.prompter.tool_calls.as_ref().map(|tc| tc.as_slice()),
		)?;

		reader
			.stream()
			.into_iter()
			.try_fold(String::new(), |acc, chunk| {
				let text = chunk?
					.choices
					.into_iter()
					.filter_map(|c| c.delta.content)
					.fold("".to_string(), |acc, s| acc + s.as_str());
				print!("{}", text);
				Ok(acc + text.as_str())
			})
	}
}
