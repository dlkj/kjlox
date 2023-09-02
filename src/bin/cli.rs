use std::{env, process::ExitCode};

use kjlox::{run_file, run_prompt};

fn main() -> ExitCode {
    let mut args = env::args();

    match (args.nth(1), args.next()) {
        (None, _) => match run_prompt() {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        },
        (Some(path), None) => match run_file(&path) {
            Ok(_) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        },
        _ => {
            println!("Usage: kjlox [script]");
            ExitCode::from(64)
        }
    }
}
