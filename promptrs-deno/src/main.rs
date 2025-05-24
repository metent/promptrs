use argh::FromArgs;
use env_logger::Target;
use log::info;
use promptrs_core::{Agent, Prompt, Prompter};
use serde::de::Error as _;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs::{OpenOptions, create_dir_all};
use std::io::{Error, Read, Write};
use std::path::PathBuf;
use std::process::{Command, ExitCode, Stdio};

fn main() -> ExitCode {
	let args = argh::from_env::<Args>();
	if args.version {
		_ = println!(
			"{} version {}",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION"),
		);
		return ExitCode::SUCCESS;
	}

	let agent = match args.subcommand {
		ArgCommand::Run(run_args) => Agent::new(run_args),
	};

	let mut data_dir = get_data_dir();
	if let Err(err) = create_dir_all(&data_dir).inspect(|_| data_dir.push("logs")) {
		eprintln!("Error creating data directory: {:?}", err);
	} else {
		match OpenOptions::new().append(true).create(true).open(&data_dir) {
			Ok(file) => env_logger::builder()
				.target(Target::Pipe(Box::new(file)))
				.filter_level(log::LevelFilter::Info)
				.init(),
			Err(err) => eprintln!("Error creating log file: {:?}", err),
		}
	}

	agent.process();
	info!("Exiting..");

	ExitCode::SUCCESS
}

#[derive(FromArgs, Serialize, Deserialize)]
/// An engine for running sandboxed AI agents over deno
pub struct Args {
	/// subcommand
	#[argh(subcommand)]
	subcommand: ArgCommand,
	/// openai server url
	#[argh(switch)]
	pub version: bool,
}

#[derive(FromArgs, Serialize, Deserialize)]
#[argh(subcommand)]
pub enum ArgCommand {
	Run(RunArgs),
}

#[derive(FromArgs, Serialize, Deserialize)]
/// Run the prompter with the given name
#[argh(subcommand, name = "run")]
pub struct RunArgs {
	/// prompter name
	#[argh(positional)]
	pub name: String,
	/// directory to sandbox; everything outside this directory will be inaccessible in the script
	#[argh(option)]
	pub dir: String,
	/// allows running subprocesses
	#[argh(option)]
	pub allow_run: Vec<String>,
}

impl Prompter for RunArgs {
	type Error = Error;

	fn run(&self, prompt: &Prompt) -> Result<Prompt, Error> {
		let mut model_path = std::fs::canonicalize(".")?;
		model_path.push(format!("{}", self.name));

		#[cfg(not(target_os = "macos"))]
		let mut command = Command::new("/usr/bin/deno");
		#[cfg(target_os = "macos")]
		let mut command = Command::new("/opt/homebrew/bin/deno");
		command
			.env("PWD", &self.dir)
			.current_dir(&self.dir)
			.arg("run")
			.arg("-R=.")
			.arg("-W=.")
			.arg(format!(
				"--allow-run={}",
				self.allow_run
					.iter()
					.fold("".to_string(), |acc, exe| acc + exe)
			))
			.arg(model_path)
			.stdin(Stdio::piped())
			.stdout(Stdio::piped())
			.stderr(Stdio::piped());
		let mut child = command.spawn()?;
		let (Some(mut stdin), Some(mut stdout), Some(mut stderr)) =
			(child.stdin.take(), child.stdout.take(), child.stderr.take())
		else {
			return Err(Error::last_os_error());
		};
		stdin.write_all(serde_json::to_string(&prompt)?.as_bytes())?;
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
}

fn get_data_dir() -> PathBuf {
	if let Ok(mut xdg_data_dirs) = env::var("XDG_DATA_DIRS") {
		xdg_data_dirs.truncate(xdg_data_dirs.find(':').unwrap_or(xdg_data_dirs.len()));
		xdg_data_dirs + "/promptrs/"
	} else if let Ok(home) = env::var("HOME") {
		home + "/.local/share/promptrs/"
	} else {
		"~/.local/share/promptrs/".into()
	}
	.into()
}
