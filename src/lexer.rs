use std::fmt::{Debug, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Punctuation
    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,
    Caret,
    Bang,

    ShiftLeft,
    ShiftRight,

    Equal,
    Unequal,

    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,

    Assign,
    Dot,
    Comma,
    Colon,
    SemiColon,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // Special Punctuation
    Arrow,
    DAmpersand,
    DPipe,
    DColon,

    // Keywords
    Fn,
    If,
    For,
    Let,
    Enum,
    Const,
    While,
    Break,
    Struct,
    Return,
    True,
    False,

    // Literals & Identifier
    Identifier(Box<str>),
    Integer(usize),
    String(Box<str>),

    // Misc
    Illegal(Box<str>),
    Eof,
}

impl TokenKind {
    /// This function unwraps the value of Illegal if the kind is already known.
    pub fn unwrap_illegal(&self) -> Box<str> {
        if let TokenKind::Illegal(msg) = self {
            msg.clone()
        } else {
            panic!("failed to unwrap TokenKind::Illegal variant");
        }
    }

    /// This function unwraps the value of Identifier if the kind is already known.
    pub fn unwrap_identifier(&self) -> Box<str> {
        if let TokenKind::Identifier(msg) = self {
            msg.clone()
        } else {
            panic!("failed to unwrap TokenKind::Identifier variant");
        }
    }
}

#[derive(Clone)]
pub struct Location {
    pub file: Box<str>,
    pub start: usize,
    pub end: usize,
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}:{}-{}\"", self.file, self.start, self.end)
    }
}

/// This function lexes a file and returns a vector of Tokens.
pub fn lex(path: &str, mut content: Peekable<Chars<'_>>) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut pos: usize = 0;

    /// This macro decides what do based on the next char.
    macro_rules! decide_on_next {
        ($cond:expr, $then:expr, $alt:expr) => {
            if content.peek().is_some_and(|c| *c == $cond) {
                content.next().unwrap();
                pos += 1;
                $then
            } else {
                $alt
            }
        };
    }

    loop {
        // skip whitespace
        while content.peek().is_some_and(|c| c.is_whitespace()) {
            content.next();
            pos += 1;
        }

        match content.next() {
            Some(c) => {
                let start_pos = pos;

                let kind = match c {
                    '+' => TokenKind::Plus,
                    '-' => decide_on_next!('>', TokenKind::Arrow, TokenKind::Minus),
                    '*' => TokenKind::Asterisk,
                    '/' => TokenKind::Slash,
                    '&' => decide_on_next!('&', TokenKind::DAmpersand, TokenKind::Ampersand),
                    '|' => decide_on_next!('|', TokenKind::DPipe, TokenKind::Pipe),
                    '^' => TokenKind::Caret,
                    '!' => decide_on_next!('=', TokenKind::Unequal, TokenKind::Bang),
                    '=' => decide_on_next!('=', TokenKind::Equal, TokenKind::Assign),
                    '<' => decide_on_next!('=', TokenKind::LessEqual,
                        decide_on_next!('<', TokenKind::ShiftLeft, TokenKind::LessThan)),
                    '>' => decide_on_next!('=', TokenKind::GreaterEqual,
                        decide_on_next!('>', TokenKind::ShiftRight, TokenKind::GreaterThan)),

                    '(' => TokenKind::LParen,
                    ')' => TokenKind::RParen,
                    '[' => TokenKind::LBracket,
                    ']' => TokenKind::RBracket,
                    '{' => TokenKind::LBrace,
                    '}' => TokenKind::RBrace,

                    '.' => TokenKind::Dot,
                    ',' => TokenKind::Comma,
                    ':' => decide_on_next!(':', TokenKind::DColon, TokenKind::Colon),
                    ';' => TokenKind::SemiColon,

                    // identifiers
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let mut ident = String::from(c);

                        while content
                            .peek()
                            .is_some_and(|c| c.is_alphanumeric() || *c == '_')
                        {
                            ident.push(content.next().unwrap());
                            pos += 1;
                        }

                        match ident.as_str() {
                            "fn" => TokenKind::Fn,
                            "if" => TokenKind::If,
                            "for" => TokenKind::For,
                            "let" => TokenKind::Let,
                            "enum" => TokenKind::Enum,
                            "while" => TokenKind::While,
                            "const" => TokenKind::Const,
                            "break" => TokenKind::Break,
                            "struct" => TokenKind::Struct,
                            "return" => TokenKind::Return,
                            "true" => TokenKind::True,
                            "false" => TokenKind::False,
                            default => TokenKind::Identifier(default.into()),
                        }
                    }

                    // integers
                    '0'..='9' => {
                        let mut number = String::from(c);

                        while content.peek().is_some_and(|c| {
                            c.is_ascii_hexdigit() || *c == 'x' || *c == 'o' || *c == 'b'
                        }) {
                            number.push(content.next().unwrap());
                            pos += 1;
                        }

                        if number.starts_with("0x") {
                            match usize::from_str_radix(&number[2..], 16) {
                                Ok(value) => TokenKind::Integer(value),
                                Err(_) => TokenKind::Illegal("Invalid integer literal.".into()),
                            }
                        } else if number.starts_with("0b") {
                            match usize::from_str_radix(&number[2..], 2) {
                                Ok(value) => TokenKind::Integer(value),
                                Err(_) => TokenKind::Illegal("Invalid integer literal.".into()),
                            }
                        } else if number.starts_with("0o") {
                            match usize::from_str_radix(&number[2..], 8) {
                                Ok(value) => TokenKind::Integer(value),
                                Err(_) => TokenKind::Illegal("Invalid integer literal.".into()),
                            }
                        } else {
                            match number.parse() {
                                Ok(value) => TokenKind::Integer(value),
                                Err(_) => TokenKind::Illegal("Invalid integer literal.".into()),
                            }
                        }
                    }

                    // strings
                    '"' => {
                        let mut value = String::new();

                        while content.peek().is_some_and(|c| *c != '"') {
                            value.push(content.next().unwrap());
                            pos += 1;
                        }

                        content.next().unwrap();

                        TokenKind::String(value.into())
                    }

                    default => TokenKind::Illegal(format!("Invalid character '{default}'.").into()),
                };

                pos += 1;

                tokens.push(Token {
                    kind,
                    loc: Location {
                        file: path.into(),
                        start: start_pos,
                        end: pos,
                    },
                });
            }
            None => break,
        }
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        loc: Location {
            file: path.into(),
            start: pos,
            end: pos,
        },
    });

    tokens
}
