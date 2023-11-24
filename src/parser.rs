use std::fmt::Display;

use crate::{
    expr::{Expr, Stmt},
    scanning::{Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug)]
pub struct ParseError(Token, String);

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(token, message) = self;
        write!(f, "Parse error: L{} {} - {message}", token.line, token.kind)
    }
}

impl std::error::Error for ParseError {}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    eprintln!("{e}");
                    self.synchronize();
                }
            }
        }

        statements
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_tokens(&[TokenKind::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let identifier = self.consume_identifier("expect variable name")?;

        if self.match_tokens(&[TokenKind::Equal]) {
            let init = self.expression()?;
            self.consume(
                &TokenKind::Semicolon,
                "expect ';' after variable declaration",
            )?;
            Ok(Stmt::Var(identifier, Some(init)))
        } else {
            self.consume(
                &TokenKind::Semicolon,
                "expect ';' after variable declaration",
            )?;
            Ok(Stmt::Var(identifier, None))
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_tokens(&[TokenKind::If]) {
            self.if_statement()
        } else if self.match_tokens(&[TokenKind::While]) {
            self.while_statement()
        } else if self.match_tokens(&[TokenKind::Print]) {
            self.print_statement()
        } else if self.match_tokens(&[TokenKind::LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenKind::LeftParen, "expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(&TokenKind::RightParen, "expect ')' after while condition")?;

        let body = Box::new(self.statement()?);

        Ok(Stmt::While(condition, body))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenKind::LeftParen, "expect '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(&TokenKind::RightParen, "expect ')' after if condition")?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = if self.match_tokens(&[TokenKind::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&TokenKind::Semicolon, "expect ';' after expression")?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&TokenKind::Semicolon, "expect ';' after expression")?;
        Ok(Stmt::Expression(value))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(&TokenKind::RightBrace, "expect '}' after block")?;

        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;

        if !self.match_tokens(&[TokenKind::Equal]) {
            return Ok(expr);
        }

        if let Expr::Identifier(s) = expr {
            Ok(Expr::Assignment(s, Box::new(self.assignment()?)))
        } else {
            Err(ParseError(
                self.previous(),
                format!("invalid assignment target {expr}"),
            ))
        }
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;
        while self.match_tokens(&[TokenKind::Or]) {
            let op = self.previous();
            let right = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                right: Box::new(right),
                op,
            }
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_tokens(&[TokenKind::And]) {
            let op = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                right: Box::new(right),
                op,
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            expr = Expr::Binary {
                left: Box::new(expr),
                op: self.previous(),
                right: Box::new(self.comparison()?),
            };
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
            expr = {
                Expr::Binary {
                    left: Box::new(expr),
                    op: self.previous(),
                    right: Box::new(self.term()?),
                }
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenKind::Minus, TokenKind::Plus]) {
            expr = {
                Expr::Binary {
                    left: Box::new(expr),
                    op: self.previous(),
                    right: Box::new(self.factor()?),
                }
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenKind::Slash, TokenKind::Star]) {
            expr = {
                Expr::Binary {
                    left: Box::new(expr),
                    op: self.previous(),
                    right: Box::new(self.unary()?),
                }
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_tokens(&[TokenKind::Bang, TokenKind::Minus]) {
            let expr = Ok({
                Expr::Unary {
                    op: self.previous(),
                    right: Box::new(self.unary()?),
                }
            });
            return expr;
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance();
        match token.kind {
            TokenKind::True => Ok(Expr::LiteralBoolean(true)),
            TokenKind::False => Ok(Expr::LiteralBoolean(false)),
            TokenKind::Nil => Ok(Expr::LiteralNil),
            TokenKind::Number(n) => Ok(Expr::LiteralNumber(n)),
            TokenKind::String(s) => Ok(Expr::LiteralString(s)),
            TokenKind::Identifier(i) => Ok(Expr::Identifier(i)),

            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.consume(&TokenKind::RightParen, "expect ')' after expression.")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }

            _ => Err(ParseError(token, "unexpected token".to_string())),
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

    fn consume(&mut self, value: &TokenKind, msg: &str) -> Result<(), ParseError> {
        if self.check(value) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError(self.peek(), msg.to_owned()))
        }
    }

    fn consume_identifier(&mut self, msg: &str) -> Result<String, ParseError> {
        if self.is_at_end() {
            return Err(ParseError(self.peek(), msg.to_owned()));
        }

        match self.peek().kind {
            TokenKind::Identifier(value) => {
                self.advance();
                Ok(value)
            }
            _ => Err(ParseError(self.peek(), msg.to_owned())),
        }
    }

    fn match_tokens(&mut self, tokens: &[TokenKind]) -> bool {
        for t in tokens {
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
        // We only care what kind of token it is
        core::mem::discriminant(&self.peek().kind) == core::mem::discriminant(value)
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
    use crate::{expr::Stmt, parser::Parser, scanning::Scanner};

    fn parse(source: &str) -> Vec<Stmt> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);

        parser.parse()
    }

    #[test]
    fn valid_group() {
        assert_eq!(
            parse("(6 / 3) - 1;")[0].to_string(),
            String::from("(Minus (group (Slash 6 3)) 1);")
        );
    }

    #[test]
    fn invalid_group() {
        assert!(parse("(6 / 3 - 1;").is_empty());
    }

    #[test]
    fn strings() {
        assert_eq!(
            parse("!(\"foo\" != \"bar\") + nil;")[0].to_string(),
            String::from("(Plus (Bang (group (BangEqual \"foo\" \"bar\"))) nil);")
        );
    }
}
