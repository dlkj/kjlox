use std::fmt::Display;

use crate::{
    expr::Expr,
    scanning::{Token, TokenKind},
};

pub struct Interpreter {}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(v) => write!(f, "{v}"),
            Self::Number(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "\"{v}\""),
        }
    }
}

#[derive(Debug)]
pub struct InterpretError {
    message: String,
}
impl InterpretError {
    fn new(message: String) -> Self {
        Self { message }
    }
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Interpret error: {}", self.message)
    }
}

impl Interpreter {
    pub fn interpret(&mut self, expr: &Expr) -> Result<Value, InterpretError> {
        self.evaluate(expr)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, InterpretError> {
        match expr {
            Expr::Binary { left, right, op } => self.binary(left, right, op),
            Expr::Grouping(e) => self.evaluate(e),
            Expr::LiteralNumber(n) => Ok(Value::Number(*n)),
            Expr::LiteralBoolean(b) => Ok(Value::Boolean(*b)),
            Expr::LiteralString(s) => Ok(Value::String(s.clone())),
            Expr::LiteralNil => Ok(Value::Nil),
            Expr::Unary { right, op } => self.unary(right, op),
        }
    }

    fn binary(&mut self, left: &Expr, right: &Expr, op: &Token) -> Result<Value, InterpretError> {
        let left_value = self.evaluate(left)?;
        let right_value = self.evaluate(right)?;

        match (&op.kind, &left_value, &right_value) {
            (TokenKind::Minus, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
            (TokenKind::Plus, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
            (TokenKind::Slash, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
            (TokenKind::Star, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
            (TokenKind::Greater, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l > r)),
            (TokenKind::GreaterEqual, Value::Number(l), Value::Number(r)) => {
                Ok(Value::Boolean(l >= r))
            }
            (TokenKind::Less, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l < r)),
            (TokenKind::LessEqual, Value::Number(l), Value::Number(r)) => {
                Ok(Value::Boolean(l <= r))
            }

            (TokenKind::Plus, Value::String(l), Value::String(r)) => {
                Ok(Value::String(format!("{l}{r}")))
            }

            (TokenKind::EqualEqual, l, r) => Ok(Value::Boolean(Self::is_equal(l, r))),
            (TokenKind::BangEqual, l, r) => Ok(Value::Boolean(!Self::is_equal(l, r))),

            _ => Err(InterpretError::new(format!(
                "invalid binary operation {left_value} '{op}' {right_value}"
            ))),
        }
    }

    fn unary(&mut self, right: &Expr, op: &Token) -> Result<Value, InterpretError> {
        let right_value = self.evaluate(right)?;

        match (&op.kind, &right_value) {
            (TokenKind::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (TokenKind::Bang, v) => Ok(Self::is_truthy(v)),

            _ => Err(InterpretError::new(format!(
                "invalid unary operation '{op}' {right_value}"
            ))),
        }
    }

    fn is_truthy(v: &Value) -> Value {
        match v {
            Value::Nil => Value::Boolean(false),
            Value::Boolean(b) => Value::Boolean(*b),
            _ => Value::Boolean(true),
        }
    }

    fn is_equal(l: &Value, r: &Value) -> bool {
        match (l, r) {
            (Value::Nil, Value::Nil) => true,
            (Value::Number(l), Value::Number(r)) => (l - r).abs() < f64::EPSILON,
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{parser::Parser, scanning::Scanner};

    use super::{Interpreter, Value};

    fn parse(source: &str) -> Option<Value> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);

        let expr = parser.parse()?;

        let mut interpreter = Interpreter {};
        interpreter.interpret(&expr).ok()
    }

    #[test]
    fn valid_maths() {
        assert_eq!(parse("(6 / 3) - 1").unwrap(), Value::Number(1.0));
    }
    
    #[test]
    fn valid_strings() {
        assert_eq!(
            parse("\"A\" + \"b\"").unwrap(),
            Value::String("Ab".to_owned())
        );
    }
}
