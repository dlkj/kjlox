use std::{
    collections::{hash_map::Entry::Occupied, HashMap},
    fmt::Display,
    io::Write,
    rc::Rc,
    time::SystemTime,
};

use crate::{
    expr::{Expr, Stmt},
    scanning::{Token, TokenKind},
};

#[derive(Debug, Default, Clone)]
struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) -> bool {
        if let Occupied(mut e) = self.values.entry(name.clone()) {
            e.insert(value);
            true
        } else if let Some(enclosing) = self.enclosing.as_mut() {
            enclosing.assign(name, value)
        } else {
            false
        }
    }

    pub fn get(&self, name: &String) -> Result<Value, InterpretError> {
        if let Some(value) = self.values.get(name).cloned() {
            Ok(value)
        } else if let Some(enclosing) = self.enclosing.as_ref() {
            enclosing.get(name)
        } else {
            Err(InterpretError(format!("undefined variable {name}")))
        }
    }
}

#[derive(Clone)]
pub enum Callable {
    Function,
    NativeFunction(usize, Rc<dyn Fn() -> Result<Value, InterpretError>>),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => todo!(),
            Self::NativeFunction(arg0, _) => f
                .debug_tuple("NativeFunction")
                .field(arg0)
                .field(&"<function pointer>")
                .finish(),
        }
    }
}
impl Callable {
    fn call(&self, _: &mut Interpreter<'_>, _: &[Value]) -> Result<Value, InterpretError> {
        match self {
            Self::Function => todo!(),
            Self::NativeFunction(_, f) => f(),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Self::Function => todo!(),
            Self::NativeFunction(n, _) => *n,
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => todo!(),
            Self::NativeFunction(_, _) => write!(f, "<native fn>"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(v) => write!(f, "{v}"),
            Self::Number(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "\"{v}\""),
            Self::Callable(v) => write!(f, "{v}"),
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

pub struct Interpreter<'a> {
    environment: Box<Environment>,
    stdout: &'a mut dyn Write,
}

impl<'a> Interpreter<'a> {
    pub fn new(out: &'a mut dyn Write) -> Interpreter<'_> {
        let mut environment = Environment::default();

        environment.define(
            "clock2".to_owned(),
            Value::Callable(Callable::NativeFunction(
                0,
                Rc::new(|| {
                    #[allow(clippy::cast_precision_loss)]
                    Ok(Value::Number(
                        SystemTime::now()
                            .duration_since(SystemTime::UNIX_EPOCH)
                            .unwrap()
                            .as_secs() as f64,
                    ))
                }),
            )),
        );

        Self {
            environment: Box::new(environment),
            stdout: out,
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), InterpretError> {
        for s in stmts {
            self.execute(s)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpretError> {
        match stmt {
            Stmt::Print(p) => self.execute_print(p),
            Stmt::Expression(s) => self.evaluate(s).map(drop),
            Stmt::Var(name, e) => self.execute_declare(e, name),
            Stmt::Block(statements) => self.execute_block(statements),
            Stmt::If(c, t, e) => self.execute_if(c, t, e.as_deref()),
            Stmt::While(c, b) => self.execute_while(c, b),
        }
    }

    fn execute_while(&mut self, condition: &Expr, body: &Stmt) -> Result<(), InterpretError> {
        while Self::is_truthy(&self.evaluate(condition)?) {
            self.execute(body)?;
        }
        Ok(())
    }

    fn execute_if(
        &mut self,
        condition: &Expr,
        then_stmt: &Stmt,
        else_stmt: Option<&Stmt>,
    ) -> Result<(), InterpretError> {
        if Self::is_truthy(&self.evaluate(condition)?) {
            self.execute(then_stmt)
        } else if let Some(else_stmt) = else_stmt {
            self.execute(else_stmt)
        } else {
            Ok(())
        }
    }

    fn execute_print(&mut self, p: &Expr) -> Result<(), InterpretError> {
        let result = self.evaluate(p)?;
        match result {
            Value::String(s) => writeln!(self.stdout, "{s}").unwrap(),
            _ => writeln!(self.stdout, "{result}").unwrap(),
        }

        Ok(())
    }

    fn execute_declare(&mut self, e: &Option<Expr>, name: &str) -> Result<(), InterpretError> {
        let value = e
            .as_ref()
            .map_or(Ok(Value::Nil), |expr| self.evaluate(expr))?;
        self.environment.define(name.to_owned(), value);
        Ok(())
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<(), InterpretError> {
        let enclosed = std::mem::take(&mut self.environment);
        self.environment.enclosing = Some(enclosed);

        let mut result = Ok(());
        for s in statements {
            if let Err(e) = self.execute(s) {
                result = Err(e);
                break;
            }
        }

        self.environment = self.environment.enclosing.take().unwrap();
        result
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
            Expr::Logical { left, right, op } => self.logical(left, right, op),
            Expr::Call(callee, token, args) => self.call(callee, token, args),
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
            (TokenKind::Bang, v) => Ok(Value::Boolean(Self::is_truthy(v))),

            _ => Err(InterpretError(format!(
                "invalid unary operation '{op}' {right_value}"
            ))),
        }
    }

    fn logical(&mut self, left: &Expr, right: &Expr, op: &Token) -> Result<Value, InterpretError> {
        let left = self.evaluate(left)?;
        match op.kind {
            TokenKind::Or => {
                if Self::is_truthy(&left) {
                    Ok(left)
                } else {
                    self.evaluate(right)
                }
            }
            TokenKind::And => {
                if Self::is_truthy(&left) {
                    self.evaluate(right)
                } else {
                    Ok(left)
                }
            }
            _ => Err(InterpretError(format!("invalid logical operation '{op}'"))),
        }
    }

    fn call(
        &mut self,
        callee: &Expr,
        token: &Token,
        args: &[Expr],
    ) -> Result<Value, InterpretError> {
        let callee = self.evaluate(callee)?;

        let evaluated_args = args
            .iter()
            .map(|e| self.evaluate(e))
            .collect::<Result<Vec<_>, _>>()?;

        match callee {
            Value::Callable(c) => {
                if evaluated_args.len() != c.arity() {
                    return Err(InterpretError(format!(
                        "line {} - expected {} arguments but got {}",
                        token.line,
                        c.arity(),
                        evaluated_args.len()
                    )));
                }
                c.call(self, &evaluated_args)
            }
            _ => Err(InterpretError(format!(
                "line {} - can only call functions and classes",
                token.line
            ))),
        }
    }

    fn is_truthy(v: &Value) -> bool {
        match v {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
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
