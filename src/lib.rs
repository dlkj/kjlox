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

pub fn run_file(out: &mut dyn Write, path: &str) -> Result<(), Error> {
    let mut file = File::open(path)?;
    let mut file_content = String::new();
    file.read_to_string(&mut file_content)?;
    run(&mut Interpreter::new(out), &file_content);
    Ok(())
}

pub fn run_prompt() -> Result<(), Error> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut interpreter = Interpreter::new(&mut stdout);

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
        run(&mut interpreter, &buffer);
    }
}

fn run(interpreter: &mut Interpreter, source: &str) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    let stmts = parser.parse();

    if stmts.is_empty() {
        return;
    };

    let value = interpreter.interpret(&stmts);

    match value {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
        }
    }
}

#[cfg(test)]
mod test {
    use std::{
        io::{Cursor, Read},
        path::Path,
    };

    use crate::run_file;

    #[test]
    fn scope() -> Result<(), crate::Error> {
        run_test(
            "./examples/scope.lox",
            r"inner a
outer b
global c
outer a
outer b
global c
global a
global b
global c
",
        )
    }

    #[test]
    fn declaration() -> Result<(), crate::Error> {
        run_test(
            "./examples/declaration.lox",
            r"before
after
3
",
        )
    }

    #[test]
    fn assignment() -> Result<(), crate::Error> {
        run_test(
            "./examples/assignment.lox",
            r"2
3
3
",
        )
    }

    #[test]
    fn expressions() -> Result<(), crate::Error> {
        run_test(
            "./examples/expressions.lox",
            r"3
2
ab
true
false
",
        )
    }

    #[test]
    fn control_flow() -> Result<(), crate::Error> {
        run_test(
            "./examples/control-flow.lox",
            r"a
0
1
2
",
        )
    }

    fn run_test(path: &str, expected: &str) -> Result<(), crate::Error> {
        let mut out = Cursor::new(vec![]);
        run_file(&mut out, Path::new(path).to_str().unwrap())?;

        let mut output = String::new();
        out.set_position(0);
        out.read_to_string(&mut output)?;

        assert_eq!(output, expected);

        Ok(())
    }
}
