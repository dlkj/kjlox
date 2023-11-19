use std::{
    collections::{hash_map::Entry::Occupied, HashMap},
    fmt::Display,
};

use crate::{
    expr::{Expr, Stmt},
    scanning::{Token, TokenKind},
};

#[derive(Debug, Default)]
pub struct Interpreter {
    environment: Environment,
}

#[derive(Debug, Default)]
struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) -> bool {
        if let Occupied(mut e) = self.values.entry(name) {
            e.insert(value);
            true
        } else {
            false
        }
    }

    pub fn get(&self, name: &String) -> Result<Value, InterpretError> {
        self.values
            .get(name)
            .cloned()
            .ok_or(InterpretError(format!("undefined variable {name}")))
    }
}

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
pub struct InterpretError(String);

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(message) = self;
        write!(f, "Interpret error: {message}")
    }
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), InterpretError> {
        for s in stmts {
            self.execute(s)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpretError> {
        match stmt {
            Stmt::Print(p) => Ok(println!("{}", self.evaluate(p)?)),
            Stmt::Expression(s) => self.evaluate(s).map(drop),
            Stmt::Var(n, e) => {
                let value = e
                    .as_ref()
                    .map_or(Ok(Value::Nil), |expr| self.evaluate(expr))?;
                self.environment.define(n.clone(), value);
                Ok(())
            }
        }
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
            Expr::Identifier(s) => self.environment.get(s).or(Ok(Value::Nil)),
            Expr::Assignment(n, e) => {
                let value = self.evaluate(e)?;
                if self.environment.assign(n.to_owned(), value.clone()) {
                    Ok(value)
                } else {
                    Err(InterpretError(format!("undefined variable {n}")))
                }
            }
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

            _ => Err(InterpretError(format!(
                "invalid binary operation {left_value} '{op}' {right_value}"
            ))),
        }
    }

    fn unary(&mut self, right: &Expr, op: &Token) -> Result<Value, InterpretError> {
        let right_value = self.evaluate(right)?;

        match (&op.kind, &right_value) {
            (TokenKind::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (TokenKind::Bang, v) => Ok(Self::is_truthy(v)),

            _ => Err(InterpretError(format!(
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
