#![warn(clippy::pedantic)]
#![warn(clippy::style)]
#![warn(clippy::use_self)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::io::{self, Read, Write};

#[derive(Debug, Clone, Copy)]
pub struct RunError;

impl Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "kjlox run error")
    }
}

impl Error for RunError {}

pub fn run_file(path: &str) -> Result<(), RunError> {
    let mut file = File::open(path).unwrap();
    let mut file_content = String::new();
    file.read_to_string(&mut file_content).unwrap();
    run(&file_content)
}

pub fn run_prompt() -> Result<(), RunError> {
    let stdin = io::stdin();
    loop {
        print!("> ");
        // flush required to ensure prompt is show immediately
        io::stdout().flush().unwrap();

        let mut buffer = String::new();
        stdin.read_line(&mut buffer).unwrap();

        // stop repl if the line is empty or contains ctrl-d
        if buffer.is_empty() || buffer.contains('\u{4}') {
            return Ok(());
        }
        run(&buffer)?;
    }
}

fn run(source: &str) -> Result<(), RunError> {
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{token}");
    }

    Ok(())
}

struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Self { source }
    }

    fn scan_tokens(&self) -> Vec<String> {
        vec![self.source.into()]
    }
}
