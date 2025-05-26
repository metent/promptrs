use argh::FromArgs;
use env_logger::Target;
use log::{error, info};
use promptrs_wasm::agent::ChatLoop;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs::{OpenOptions, create_dir_all};
use std::path::PathBuf;
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

	let ArgCommand::Run(RunArgs { path, host_dir }) = args.subcommand;

	if let Err(err) = ChatLoop::new(path, host_dir).and_then(|agent| agent.process()) {
		error!("Fatal Error: {err}");
	}
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
	/// path of wasm file
	#[argh(positional)]
	pub path: String,
	/// sandboxed host directory
	#[argh(positional)]
	pub host_dir: String,
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
