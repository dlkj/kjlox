use crate::scanning::Token;

#[derive(Debug, PartialEq)]
enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: Token,
    },
    Grouping(Box<Expr>),
    NumberLiteral(f64),
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
            Self::NumberLiteral(value) => write!(f, "{value}"),
            Self::Unary { right, op } => write!(f, "({op} {})", &right),
        }
    }
}
impl Expr {
    fn new_binary(left: Self, op: Token, right: Self) -> Self {
        Self::Binary {
            left: Box::new(left),
            right: Box::new(right),
            op,
        }
    }

    fn new_number_literal(value: f64) -> Self {
        Self::NumberLiteral(value)
    }

    fn new_grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }

    fn new_unary(op: Token, right: Self) -> Self {
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
                Expr::new_number_literal(123.0),
            ),
            Token {
                kind: TokenKind::Star,
                line: 1,
            },
            Expr::new_grouping(Expr::new_number_literal(45.67)),
        );

        assert_eq!(
            expr.to_string(),
            String::from("(Star (Minus 123) (group 45.67))")
        );
    }
}
