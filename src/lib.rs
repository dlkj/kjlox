#![warn(clippy::pedantic)]
#![warn(clippy::style)]
#![warn(clippy::use_self)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

use std::fmt::Display;
use std::fs::File;
use std::io::{self, Read, Write};

use interpreter::Interpreter;
use parser::Parser;
use scanning::Scanner;

mod expr;
mod interpreter;
mod parser;
mod scanning;

#[derive(Debug)]
pub enum Error {
    Run {
        line: usize,
        location: String,
        message: String,
    },
    Io(std::io::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Run {
                line,
                location,
                message,
            } => write!(f, "[line {line}] Error{location}: {message}"),
            Self::Io(e) => write!(f, "IO Error: {e}"),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    #[must_use]
    pub fn run(line: usize, location: String, message: String) -> Self {
        Self::Run {
            line,
            location,
            message,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

pub fn run_file(path: &str) -> Result<(), Error> {
    let mut file = File::open(path)?;
    let mut file_content = String::new();
    file.read_to_string(&mut file_content)?;
    run(&file_content);
    Ok(())
}

pub fn run_prompt() -> Result<(), Error> {
    let stdin = io::stdin();
    loop {
        print!("> ");
        // flush required to ensure prompt is show immediately
        io::stdout().flush()?;

        let mut buffer = String::new();
        stdin.read_line(&mut buffer)?;

        // stop repl if the line is empty or contains ctrl-d
        if buffer.is_empty() || buffer.contains('\u{4}') {
            return Ok(());
        }
        run(&buffer);
    }
}

fn run(source: &str) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    let expr = parser.parse();

    let Some(expr) = expr else { return };

    let mut interpreter = Interpreter {};
    let value = interpreter.interpret(&expr);

    let Ok(value) = value else { return };

    print!("{value}");
}
