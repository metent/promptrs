use crate::openai::Message;
use serde::de::Error;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::io::{self, Read, Result, Write};
use std::process::{Command, Stdio};

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Prompter {
	#[serde(skip_serializing_if = "Option::is_none")]
	pub system: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub response: Option<String>,
	pub context: Vec<String>,
	pub assistant: Vec<String>,
	pub tool: Vec<String>,
	pub user: Vec<String>,
	pub status: Vec<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub base_url: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub model: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub api_token: Option<String>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub from_agent: Option<String>,
	pub to_agent: String,
	pub tool_calls: Option<Vec<Value>>,
}

impl Prompter {
	pub fn new(name: String) -> Prompter {
		Prompter {
			system: None,
			response: None,
			context: vec![],
			assistant: vec![],
			tool: vec![],
			user: vec![],
			status: vec![],
			base_url: None,
			model: None,
			api_token: None,
			from_agent: None,
			to_agent: name,
			tool_calls: None,
		}
	}

	pub fn run(&self, dir: &str, allowed: &[String]) -> Result<Prompter> {
		let mut model_path = std::fs::canonicalize(".")?;
		model_path.push(format!("{}.ts", self.to_agent));

		#[cfg(not(target_os = "macos"))]
		let mut command = Command::new("/usr/bin/deno");
		#[cfg(target_os = "macos")]
		let mut command = Command::new("/opt/homebrew/bin/deno");
		command
			.env("PWD", dir)
			.current_dir(dir)
			.arg("run")
			.arg("-R=.")
			.arg("-W=.")
			.arg(format!(
				"--allow-run={}",
				allowed.iter().fold("".to_string(), |acc, exe| acc + exe)
			))
			.arg(model_path)
			.stdin(Stdio::piped())
			.stdout(Stdio::piped())
			.stderr(Stdio::piped());
		let mut child = command.spawn()?;
		let (Some(mut stdin), Some(mut stdout), Some(mut stderr)) =
			(child.stdin.take(), child.stdout.take(), child.stderr.take())
		else {
			return Err(io::Error::last_os_error());
		};
		stdin.write_all(serde_json::to_string(&self)?.as_bytes())?;
		drop(stdin);

		let buffer = child
			.wait()?
			.success()
			.then(|| {
				let mut buffer = String::new();
				_ = stdout.read_to_string(&mut buffer);
				buffer
			})
			.unwrap_or_else(|| {
				let mut buffer = String::new();
				buffer += "\n\nCommand failed with stderr:\n\n";
				_ = stderr.read_to_string(&mut buffer);
				buffer += "\n\nCommand failed with stdout:\n\n";
				_ = stdout.read_to_string(&mut buffer);
				buffer
			});

		Ok(serde_json::from_str(&buffer)
			.map_err(|err| serde_json::Error::custom(buffer + "\n" + &err.to_string()))?)
	}

	pub fn messages(&self) -> Vec<Message<'_>> {
		let iter = self
			.context
			.iter()
			.zip(self.tool.iter())
			.zip(self.user.iter())
			.zip(self.status.iter())
			.zip(self.assistant.iter())
			.map(|((((ctx, tool), user), status), assistant)| {
				[
					Some(Message::Assistant {
						content: ctx.to_string() + assistant,
					}),
					tool.ne("").then_some(Message::Tool {
						tool_call_id: "last",
						content: tool,
					}),
					(user.ne("") || status.ne("")).then_some(Message::User {
						content: user.to_string() + status,
					}),
				]
			})
			.flatten()
			.flatten();
		self.system
			.iter()
			.map(|sys| {
				[
					Message::System { content: sys },
					Message::User { content: "".into() },
				]
			})
			.flatten()
			.chain(iter)
			.collect()
	}
}
