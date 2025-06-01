use argh::FromArgs;
use eframe::App;
use eframe::glow::Context;
use env_logger::Target;
use log::{error, info, warn};
use notify::{Event, EventKind, RecursiveMode, Watcher};
use promptrs::agent::ChatLoop;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs::{OpenOptions, create_dir_all, read_to_string, write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::mpsc;
use std::thread;

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
	let mut cache_dir = None;
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
		data_dir.pop();
		data_dir.push("cache");
		cache_dir = Some(data_dir);
	}

	let ArgCommand::Run(RunArgs {
		path,
		host_dir,
		prompt_file,
	}) = args.subcommand;

	if let Some(file) = prompt_file {
		thread::spawn(move || agent_thread(path, host_dir, cache_dir));
		watch_thread(file);
	} else {
		agent_thread(path, host_dir, cache_dir);
	}
	info!("Exiting..");

	ExitCode::SUCCESS
}

fn agent_thread(path: String, host_dir: String, cache_dir: Option<PathBuf>) {
	match ChatLoop::new(path, host_dir, cache_dir).and_then(|agent| agent.process()) {
		Ok(_) => info!("Agent thread finished execution."),
		Err(err) => error!("Failed to run agent thread: {err}"),
	}
}

fn watch_thread(file: String) {
	match PromptGui::new(file).event_loop() {
		Ok(_) => info!("Watch thread finished execution."),
		Err(err) => warn!("Failed to run watch thread: {err}"),
	}
}

#[derive(Default, Clone)]
struct PromptGui {
	code: String,
	prompt_file: String,
}

impl PromptGui {
	fn new(prompt_file: String) -> Self {
		PromptGui {
			code: "".into(),
			prompt_file,
		}
	}

	fn event_loop(&mut self) -> notify::Result<()> {
		let (tx, rx) = mpsc::channel();
		let mut watcher = notify::recommended_watcher(tx)?;
		watcher.watch(Path::new(&self.prompt_file), RecursiveMode::NonRecursive)?;
		for res in rx {
			match res {
				Ok(Event {
					kind: EventKind::Modify(_),
					..
				}) => {
					if let Err(err) = eframe::run_native(
						"promptrs",
						eframe::NativeOptions::default(),
						Box::new(|_| {
							self.code = read_to_string(&self.prompt_file)?;
							Ok(Box::new(self.clone()))
						}),
					) {
						warn!("{}", err);
					}
				}
				Err(err) => warn!("{}", err),
				_ => {}
			}
		}
		Ok(())
	}
}

impl App for PromptGui {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		egui::CentralPanel::default().show(ctx, |ui| {
			ui.horizontal(|ui| {
				ui.set_height(0.0);
				ui.label("An example of syntax highlighting in a TextEdit.");
			});

			let theme =
				egui_extras::syntax_highlighting::CodeTheme::from_memory(ui.ctx(), ui.style());

			let mut layouter = |ui: &egui::Ui, buf: &str, wrap_width: f32| {
				let mut layout_job = egui_extras::syntax_highlighting::highlight(
					ui.ctx(),
					ui.style(),
					&theme,
					buf,
					"markdown",
				);
				layout_job.wrap.max_width = wrap_width;
				ui.fonts(|f| f.layout_job(layout_job))
			};

			egui::ScrollArea::vertical().show(ui, |ui| {
				ui.add(
					egui::TextEdit::multiline(&mut self.code)
						.font(egui::TextStyle::Monospace)
						.code_editor()
						.desired_rows(10)
						.lock_focus(true)
						.desired_width(f32::INFINITY)
						.layouter(&mut layouter),
				);
			});
		});
	}

	fn on_exit(&mut self, _gl: Option<&Context>) {
		if let Err(err) = write(&self.prompt_file, &self.code) {
			warn!("Failed to write contents. Error: {err}");
		}
	}
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
	/// prompt file
	#[argh(option)]
	pub prompt_file: Option<String>,
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
