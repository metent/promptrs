use crate::openai::{ChatReader, OpenAIError};
use crate::prompt::{Prompt, Prompter};
use log::{info, warn};
use std::thread::sleep;
use std::time::Duration;

pub struct Agent<P: Prompter> {
	prompter: P,
	prompt: Prompt,
}

impl<P: Prompter> Agent<P> {
	pub fn new(prompter: P) -> Self {
		Self {
			prompter,
			prompt: Prompt::default(),
		}
	}

	pub fn process(mut self) {
		loop {
			self.prompt = match self.prompter.run(&self.prompt) {
				Ok(mut res) => {
					info!("\n\nGenerated prompt: {:?}", res);
					_ = res.from_agent.insert(self.prompt.to_agent);
					res
				}
				Err(err) => {
					warn!("\n\nPrompt Generation Failed: {}", err);
					self.prompt
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

			_ = self.prompt.response.insert(raw_response);
		}
	}

	fn chat_completion(&self) -> Result<String, OpenAIError> {
		let reader = ChatReader::new(
			&self.prompt.model.as_ref().map_or("", |m| m.as_str()),
			&self.prompter.messages(&self.prompt),
			self.prompt
				.base_url
				.as_ref()
				.map(|u| u.as_str())
				.unwrap_or(""),
			self.prompt.api_token.as_ref().map(|t| t.as_str()),
			self.prompt.tool_calls.as_ref().map(|tc| tc.as_slice()),
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
