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
        write!(
            f,
            "line {} - parse error: {} - {message}",
            token.line, token.kind
        )
    }
}

impl std::error::Error for ParseError {}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];

        let mut error = None;

        while !self.is_at_end() {
            match self.declaration() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    eprintln!("{e}");
                    if error.is_none() {
                        error = Some(e);
                    }
                    self.synchronize();
                }
            }
        }

        if let Some(error) = error {
            Err(error)
        } else {
            Ok(statements)
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_tokens(&[TokenKind::Fun]) {
            self.function()
        } else if self.match_tokens(&[TokenKind::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn function(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume_identifier("Expect function name")?;
        self.consume(&TokenKind::LeftParen, "Expect '(' after function name")?;

        let mut params = vec![];
        if !self.check(&TokenKind::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(ParseError(
                        self.peek(),
                        "Can't have more than 255 parameters.".to_owned(),
                    ));
                }
                params.push(self.consume_identifier("Expect parameter name")?);
                if !self.match_tokens(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightParen, "Expect ')' after function params")?;

        self.consume(&TokenKind::LeftBrace, "Expect '{' before function body")?;
        let body = self.block()?;
        Ok(Stmt::Function(name, params, body))
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
        } else if self.match_tokens(&[TokenKind::For]) {
            self.for_statement()
        } else if self.match_tokens(&[TokenKind::Print]) {
            self.print_statement()
        } else if self.match_tokens(&[TokenKind::Return]) {
            self.return_statement()
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

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenKind::LeftParen, "expect '(' after 'for'")?;

        let init = if self.match_tokens(&[TokenKind::Semicolon]) {
            None
        } else if self.match_tokens(&[TokenKind::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.match_tokens(&[TokenKind::Semicolon]) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(&TokenKind::Semicolon, "expect ';' after for condition")?;

        let update = if self.match_tokens(&[TokenKind::Semicolon]) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(&TokenKind::RightParen, "expect ')' after for clause")?;

        let body = self.statement()?;

        let while_stmt = Stmt::While(
            condition.unwrap_or(Expr::LiteralBoolean(true)),
            Box::new(match update {
                Some(update) => Stmt::Block(vec![body, Stmt::Expression(update)]),
                _ => body,
            }),
        );

        Ok(match init {
            Some(init) => Stmt::Block(vec![init, while_stmt]),
            _ => while_stmt,
        })
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

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&TokenKind::Semicolon, "expect ';' after return")?;
        Ok(Stmt::Return(value))
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

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_tokens(&[TokenKind::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = vec![];
        if !self.check(&TokenKind::RightParen) {
            loop {
                if args.len() >= 255 {
                    return Err(ParseError(
                        self.peek(),
                        "Can't have more than 255 arguments.".to_owned(),
                    ));
                }
                args.push(self.expression()?);
                if !self.match_tokens(&[TokenKind::Comma]) {
                    break;
                }
            }
        }
        let paren = self.consume(&TokenKind::RightParen, "expect ')' after call args")?;
        Ok(Expr::Call(Box::new(callee), paren, args))
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

    fn consume(&mut self, value: &TokenKind, msg: &str) -> Result<Token, ParseError> {
        if self.check(value) {
            Ok(self.advance())
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

    use super::ParseError;

    fn parse(source: &str) -> Result<Vec<Stmt>, ParseError> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);

        parser.parse()
    }

    #[test]
    fn valid_group() {
        assert_eq!(
            parse("(6 / 3) - 1;").unwrap()[0].to_string(),
            String::from("(Minus (group (Slash 6 3)) 1);")
        );
    }

    #[test]
    fn invalid_group() {
        assert!(parse("(6 / 3 - 1;").is_err());
    }

    #[test]
    fn strings() {
        assert_eq!(
            parse("!(\"foo\" != \"bar\") + nil;").unwrap()[0].to_string(),
            String::from("(Plus (Bang (group (BangEqual \"foo\" \"bar\"))) nil);")
        );
    }
}
