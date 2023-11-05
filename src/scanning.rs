use std::fmt::Display;

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    errors: Vec<ScanError>,
    start: usize,
    current: usize,
    line: usize,
}
#[derive(Debug)]
pub struct ScanError {
    line: usize,
    message: String,
}

impl Display for ScanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] scan Error: {}", self.line, self.message)
    }
}

impl std::error::Error for ScanError {}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(), // todo: make this lazy, quite wasteful of memory
            tokens: vec![],
            errors: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            kind: TokenKind::Eof,
            line: self.line,
        });
        self.tokens.clone()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            '-' => self.add_token(TokenKind::Minus),
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            '*' => self.add_token(TokenKind::Star),
            '!' => {
                if self.try_advance_with('=') {
                    self.add_token(TokenKind::BangEqual);
                } else {
                    self.add_token(TokenKind::Bang);
                }
            }
            '=' => {
                if self.try_advance_with('=') {
                    self.add_token(TokenKind::EqualEqual);
                } else {
                    self.add_token(TokenKind::Equal);
                }
            }
            '<' => {
                if self.try_advance_with('=') {
                    self.add_token(TokenKind::LessEqual);
                } else {
                    self.add_token(TokenKind::Less);
                }
            }
            '>' => {
                if self.try_advance_with('=') {
                    self.add_token(TokenKind::GreaterEqual);
                } else {
                    self.add_token(TokenKind::Greater);
                }
            }
            '/' => {
                if self.try_advance_with('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            c => self.add_error(format!("Unexpected character '{c}'")),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    fn peek(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&mut self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn try_advance_with(&mut self, c: char) -> bool {
        if self.is_at_end() || self.source[self.current] != c {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            line: self.line,
        });
    }

    fn add_error(&mut self, message: String) {
        let error = ScanError {
            line: self.line,
            message,
        };
        println!("{error}");
        self.errors.push(error);
    }

    fn string(&mut self) {
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.add_error("Unterminated string".to_string());
        }

        self.advance();
        let value: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token(TokenKind::String(value));
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let value: String = self.source[self.start..self.current].iter().collect();
        self.add_token(TokenKind::Number(value.parse::<f64>().unwrap()));
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        let value: String = self.source[self.start..self.current].iter().collect();

        match value.as_str() {
            "and" => self.add_token(TokenKind::And),
            "class" => self.add_token(TokenKind::Class),
            "else" => self.add_token(TokenKind::Else),
            "false" => self.add_token(TokenKind::False),
            "for" => self.add_token(TokenKind::For),
            "fun" => self.add_token(TokenKind::Fun),
            "if" => self.add_token(TokenKind::If),
            "nil" => self.add_token(TokenKind::Nil),
            "or" => self.add_token(TokenKind::Or),
            "print" => self.add_token(TokenKind::Print),
            "return" => self.add_token(TokenKind::Return),
            "super" => self.add_token(TokenKind::Super),
            "this" => self.add_token(TokenKind::This),
            "true" => self.add_token(TokenKind::True),
            "var" => self.add_token(TokenKind::Var),
            "while" => self.add_token(TokenKind::While),
            _ => self.add_token(TokenKind::Identifier(value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftParen => write!(f, "LeftParen"),
            Self::RightParen => write!(f, "RightParen"),
            Self::LeftBrace => write!(f, "LeftBrace"),
            Self::RightBrace => write!(f, "RightBrace"),
            Self::Comma => write!(f, "Comma"),
            Self::Dot => write!(f, "Dot"),
            Self::Minus => write!(f, "Minus"),
            Self::Plus => write!(f, "Plus"),
            Self::Semicolon => write!(f, "Semicolon"),
            Self::Slash => write!(f, "Slash"),
            Self::Star => write!(f, "Star"),
            Self::Bang => write!(f, "Bang"),
            Self::BangEqual => write!(f, "BangEqual"),
            Self::Equal => write!(f, "Equal"),
            Self::EqualEqual => write!(f, "EqualEqual"),
            Self::Greater => write!(f, "Greater"),
            Self::GreaterEqual => write!(f, "GreaterEqual"),
            Self::Less => write!(f, "Less"),
            Self::LessEqual => write!(f, "LessEqual"),
            Self::Identifier(s) => write!(f, "Identifier({s})"),
            Self::String(s) => write!(f, "String({s})"),
            Self::Number(n) => write!(f, "Number({n})"),
            Self::And => write!(f, "And"),
            Self::Class => write!(f, "Class"),
            Self::Else => write!(f, "Else"),
            Self::False => write!(f, "False"),
            Self::Fun => write!(f, "Fun"),
            Self::For => write!(f, "For"),
            Self::If => write!(f, "If"),
            Self::Nil => write!(f, "Nil"),
            Self::Or => write!(f, "Or"),
            Self::Print => write!(f, "Print"),
            Self::Return => write!(f, "Return"),
            Self::Super => write!(f, "Super"),
            Self::This => write!(f, "This"),
            Self::True => write!(f, "True"),
            Self::Var => write!(f, "Var"),
            Self::While => write!(f, "While"),
            Self::Eof => write!(f, "Eof"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::scanning::{Token, TokenKind};

    use super::Scanner;

    #[test]
    fn basic_tokens() {
        let input = "// this is a comment
        (( )){} // grouping stuff
        !*+-/=<> <= == // operators
        ; //terminator";

        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::LeftParen,
                    line: 2
                },
                Token {
                    kind: TokenKind::LeftParen,
                    line: 2
                },
                Token {
                    kind: TokenKind::RightParen,
                    line: 2
                },
                Token {
                    kind: TokenKind::RightParen,
                    line: 2
                },
                Token {
                    kind: TokenKind::LeftBrace,
                    line: 2
                },
                Token {
                    kind: TokenKind::RightBrace,
                    line: 2
                },
                Token {
                    kind: TokenKind::Bang,
                    line: 3
                },
                Token {
                    kind: TokenKind::Star,
                    line: 3
                },
                Token {
                    kind: TokenKind::Plus,
                    line: 3
                },
                Token {
                    kind: TokenKind::Minus,
                    line: 3
                },
                Token {
                    kind: TokenKind::Slash,
                    line: 3
                },
                Token {
                    kind: TokenKind::Equal,
                    line: 3
                },
                Token {
                    kind: TokenKind::Less,
                    line: 3
                },
                Token {
                    kind: TokenKind::Greater,
                    line: 3
                },
                Token {
                    kind: TokenKind::LessEqual,
                    line: 3
                },
                Token {
                    kind: TokenKind::EqualEqual,
                    line: 3
                },
                Token {
                    kind: TokenKind::Semicolon,
                    line: 4
                },
                Token {
                    kind: TokenKind::Eof,
                    line: 4
                }
            ]
        );
    }

    #[test]
    fn string_token() {
        let input = "\"test string\"";

        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::String("test string".into()),
                    line: 1
                },
                Token {
                    kind: TokenKind::Eof,
                    line: 1
                }
            ]
        );
    }

    #[test]
    fn number_token() {
        let input = "1234 12.34";

        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::Number(1234.0),
                    line: 1
                },
                Token {
                    kind: TokenKind::Number(12.34),
                    line: 1
                },
                Token {
                    kind: TokenKind::Eof,
                    line: 1
                }
            ]
        );
    }

    #[test]
    fn identifier_token() {
        let input = "identifier token and return
        or else";

        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::Identifier("identifier".into()),
                    line: 1
                },
                Token {
                    kind: TokenKind::Identifier("token".into()),
                    line: 1
                },
                Token {
                    kind: TokenKind::And,
                    line: 1
                },
                Token {
                    kind: TokenKind::Return,
                    line: 1
                },
                Token {
                    kind: TokenKind::Or,
                    line: 2
                },
                Token {
                    kind: TokenKind::Else,
                    line: 2
                },
                Token {
                    kind: TokenKind::Eof,
                    line: 2
                }
            ]
        );
    }
}
