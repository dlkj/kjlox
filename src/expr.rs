use crate::scanning::Token;

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
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary { left, right, op } => write!(f, "({op} {} {})", &left, &right),
            Self::Grouping(e) => write!(f, "(group {})", &e),
            Self::LiteralString(value) => write!(f, "\"{value}\""),
            Self::LiteralNumber(value) => write!(f, "{value}"),
            Self::LiteralBoolean(value) => write!(f, "{value}"),
            Self::Unary { right, op } => write!(f, "({op} {})", &right),
            Self::LiteralNil => write!(f, "nil"),
        }
    }
}
impl Expr {
    pub fn new_binary(left: Self, op: Token, right: Self) -> Self {
        Self::Binary {
            left: Box::new(left),
            right: Box::new(right),
            op,
        }
    }

    pub fn new_literal_number(value: f64) -> Self {
        Self::LiteralNumber(value)
    }

    pub fn new_literal_string(value: String) -> Self {
        Self::LiteralString(value)
    }

    pub fn new_literal_boolean(value: bool) -> Self {
        Self::LiteralBoolean(value)
    }

    pub fn new_literal_nil() -> Self {
        Self::LiteralNil
    }

    pub fn new_grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }

    pub fn new_unary(op: Token, right: Self) -> Self {
        Self::Unary {
            right: Box::new(right),
            op,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::scanning::{Token, TokenKind};

    use super::Expr;

    #[test]
    fn print_ast() {
        let expr = Expr::new_binary(
            Expr::new_unary(
                Token {
                    kind: TokenKind::Minus,
                    line: 1,
                },
                Expr::new_literal_number(123.0),
            ),
            Token {
                kind: TokenKind::Star,
                line: 1,
            },
            Expr::new_grouping(Expr::new_literal_number(45.67)),
        );

        assert_eq!(
            expr.to_string(),
            String::from("(Star (Minus 123) (group 45.67))")
        );
    }
}
