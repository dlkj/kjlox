use std::fmt::Display;

use crate::{
    expr::Expr,
    scanning::{Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}
impl ParseError {
    fn new(message: String) -> Self {
        Self { message }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        self.expression()
            .map_err(|e| {
                eprintln!("{e}");
            })
            .ok()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Expr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term()?;
            expr = Expr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenKind::Minus, TokenKind::Plus]) {
            let op = self.previous();
            let right = self.factor()?;
            expr = Expr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenKind::Slash, TokenKind::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::new_binary(expr, op, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_tokens(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.previous();
            let right = self.unary()?;
            return Ok(Expr::new_unary(op, right));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance();
        match token.kind {
            TokenKind::True => Ok(Expr::new_literal_boolean(true)),
            TokenKind::False => Ok(Expr::new_literal_boolean(false)),
            TokenKind::Nil => Ok(Expr::new_literal_nil()),
            TokenKind::Number(n) => Ok(Expr::new_literal_number(n)),
            TokenKind::String(s) => Ok(Expr::new_literal_string(s)),

            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::new_grouping(expr))
            }

            token => Err(ParseError::new(format!("Unexpected token {token}"))),
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            match self.peek().kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::Print
                | TokenKind::Return => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn consume(&mut self, value: TokenKind, msg: &str) -> Result<(), ParseError> {
        if self.check(&value) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(msg.to_owned()))
        }
    }

    fn match_tokens(&mut self, tokens: &[TokenKind]) -> bool {
        for t in tokens.iter() {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, value: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().kind == value
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn peek(&mut self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
}

#[cfg(test)]
mod test {
    use crate::{parser::Parser, scanning::Scanner};

    fn parse(source: &str) -> Option<crate::expr::Expr> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);

        parser.parse()
    }

    #[test]
    fn valid_group() {
        assert_eq!(
            parse("(6 / 3) - 1").unwrap().to_string(),
            String::from("(Minus (group (Slash 6 3)) 1)")
        );
    }

    #[test]
    fn invalid_group() {
        assert_eq!(parse("(6 / 3 - 1"), None);
    }

    #[test]
    fn strings() {
        assert_eq!(
            parse("!(\"foo\" != \"bar\") + nil").unwrap().to_string(),
            String::from("(Plus (Bang (group (BangEqual \"foo\" \"bar\"))) nil)")
        );
    }
}
