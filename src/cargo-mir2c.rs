// warn on lints, that are included in `rust-lang/rust`s bootstrap
#![warn(rust_2018_idioms, unused_lifetimes)]

// ADAPTED FROM https://github.com/rust-lang/rust-clippy/

use rustc_tools_util::VersionInfo;
use std::env;
use std::path::PathBuf;
use std::process::{self, Command};

// TODO actually generate C11 compliant code instead of relying on impl dependent features
const CARGO_HELP: &str = r#"Compiles your Rust code and convert it to C11 compliant code.
Usage:
    cargo mir2c [options] [--] [<opts>...]
Common options:
    -h, --help               Print this message
    -V, --version            Print version info and exit
"#;

fn show_help() {
    println!("{}", CARGO_HELP);
}

fn show_version() {
    let version_info = rustc_tools_util::get_version_info!();
    println!("{}", version_info);
}

pub fn main() {
    // Check for version and help flags even when invoked as 'cargo-clippy'
    if env::args().any(|a| a == "--help" || a == "-h") {
        show_help();
        return;
    }

    if env::args().any(|a| a == "--version" || a == "-V") {
        show_version();
        return;
    }

    if let Err(code) = process(env::args().skip(2)) {
        process::exit(code);
    }
}

struct Mir2cCmd {
    cargo_subcommand: &'static str,
    args: Vec<String>,
}

impl Mir2cCmd {
    fn new<I>(mut old_args: I) -> Self
    where
        I: Iterator<Item = String>,
    {
        let cargo_subcommand = "rustc";
        let mut args = vec![];
        let mut mir2c_args: Vec<String> = vec![];

        for arg in old_args.by_ref() {
            // TODO start accepting arguments when we have arguments.
            if arg.as_str() == "--" {
                break;
            }

            args.push(arg);
        }

        mir2c_args.append(&mut (old_args.collect()));

        Self {
            cargo_subcommand,
            args,
        }
    }

    fn path() -> PathBuf {
        let mut path = env::current_exe()
            .expect("current executable path invalid")
            .with_file_name("mir2c");

        if cfg!(windows) {
            path.set_extension("exe");
        }

        path
    }

    fn into_std_cmd(self) -> Command {
        let mut cmd = Command::new("cargo");

        cmd.env("RUSTC_WORKSPACE_WRAPPER", Self::path())
            .arg(self.cargo_subcommand)
            .args(&self.args);

        cmd
    }
}

fn process<I>(old_args: I) -> Result<(), i32>
where
    I: Iterator<Item = String>,
{
    let cmd = Mir2cCmd::new(old_args);

    let mut cmd = cmd.into_std_cmd();

    let exit_status = cmd
        .spawn()
        .expect("could not run cargo")
        .wait()
        .expect("failed to wait for cargo?");

    if exit_status.success() {
        Ok(())
    } else {
        Err(exit_status.code().unwrap_or(-1))
    }
}
