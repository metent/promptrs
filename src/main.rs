mod agent;
mod openai;
mod prompt;

use agent::Agent;
use argh::FromArgs;
use log::error;
use serde::{Deserialize, Serialize};
use std::process::ExitCode;

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
		Command::Run(run_args) => Agent::new(run_args.name, run_args.dir, run_args.allow_run),
	};
	let agent = match agent {
		Ok(agent) => agent,
		Err(err) => {
			error!("{err}");
			return ExitCode::FAILURE;
		}
	};
	agent.process();

	ExitCode::SUCCESS
}

#[derive(FromArgs, Serialize, Deserialize)]
/// An engine for running sandboxed AI agents over deno
pub struct Args {
	/// subcommand
	#[argh(subcommand)]
	subcommand: Command,
	/// openai server url
	#[argh(switch)]
	pub version: bool,
}

#[derive(FromArgs, Serialize, Deserialize)]
#[argh(subcommand)]
pub enum Command {
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
