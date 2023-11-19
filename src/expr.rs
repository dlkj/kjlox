use crate::scanning::Token;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    Var(String, Option<Expr>),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Print(expr) => write!(f, "print {expr};"),
            Self::Expression(expr) => write!(f, "{expr};"),
            Self::Var(ident, expr) => match expr {
                Some(e) => write!(f, "var {ident} = {e};"),
                None => write!(f, "var {ident};"),
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: Token,
    },
    Grouping(Box<Expr>),
    LiteralNumber(f64),
    LiteralBoolean(bool),
    LiteralString(String),
    LiteralNil,
    Unary {
        right: Box<Expr>,
        op: Token,
    },
    Identifier(String),
    Assignment(String, Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary { left, right, op } => write!(f, "({op} {} {})", &left, &right),
            Self::Grouping(e) => write!(f, "(group {})", &e),
            Self::LiteralString(value) => write!(f, "\"{value}\""),
            Self::LiteralNumber(value) => write!(f, "{value}"),
            Self::LiteralBoolean(value) => write!(f, "{value}"),
            Self::Unary { right, op } => write!(f, "({op} {right})"),
            Self::LiteralNil => write!(f, "nil"),
            Self::Identifier(s) => write!(f, "{s}"),
            Self::Assignment(s, v) => write!(f, "{s} = {v}"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::scanning::{Token, TokenKind};

    use super::Expr;

    #[test]
    fn print_ast() {
        let expr = {
            let left = {
                let op = Token {
                    kind: TokenKind::Minus,
                    line: 1,
                };
                let right = {
                    let value = 123.0;
                    Expr::LiteralNumber(value)
                };
                Expr::Unary {
                    right: Box::new(right),
                    op,
                }
            };
            let op = Token {
                kind: TokenKind::Star,
                line: 1,
            };
            let right = {
                let expr = {
                    let value = 45.67;
                    Expr::LiteralNumber(value)
                };
                Expr::Grouping(Box::new(expr))
            };
            Expr::Binary {
                left: Box::new(left),
                right: Box::new(right),
                op,
            }
        };

        assert_eq!(
            expr.to_string(),
            String::from("(Star (Minus 123) (group 45.67))")
        );
    }
}
