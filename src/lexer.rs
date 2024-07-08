use core::fmt;
use std::str::Chars;

use thiserror::Error;

#[derive(Error, Clone, Debug, PartialEq, Eq)]
pub enum LexError {
    #[error("unexpected character")]
    UnexpectedChar,
    #[error("invalid identifier")]
    InvalidIdentifier,
    #[error("unterminated string")]
    UntermindatedString,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenType,
    pub start: usize,
    pub end: usize,
    pub value: TokenValue,
}

impl Token {
    fn new(kind: TokenType, start: usize, end: usize, value: TokenValue) -> Self {
        Self {
            kind,
            start,
            end,
            value,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "TokenType: {:?}, Value: {:?}, Location: {}-{}",
            self.kind, self.value, self.start, self.end
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenValue {
    None,
    Integer(i32),
    String(String),
    Error(LexError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    Identifier,
    Int,
    Void,
    Return,
    Constant,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Whitespace,
    Eof,
    InvalidIdent,
    Unknown,
}

const EOF: char = '\0';

pub struct Lexer<'a> {
    /// Source Text
    source: &'a str,

    /// Remaining source characters
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
        }
    }

    pub fn tokenize(&'a mut self) -> impl Iterator<Item = Token> + '_ {
        std::iter::from_fn(move || {
            let token = self.scan_token();
            if token.kind != TokenType::Eof {
                Some(token)
            } else {
                None
            }
        })
    }

    fn scan_token(&mut self) -> Token {
        let start = self.offset();

        let c = match self.advance() {
            Some(c) => c,
            None => return Token::new(TokenType::Eof, start, self.offset(), TokenValue::None),
        };

        let token_type = match c {
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            ';' => TokenType::Semicolon,
            _c @ '0'..='9' => self.number(),
            _c @ 'a'..='z' | _c @ 'A'..='Z' | _c @ '_' => self.identifier(start),
            ' ' | '\r' | '\t' | '\n' => TokenType::Whitespace,
            _ => TokenType::Unknown,
        };

        let end = self.offset();

        let token_value = match token_type {
            TokenType::Constant => {
                TokenValue::Integer(self.source[start..end].parse::<i32>().unwrap())
            }
            TokenType::Unknown => TokenValue::Error(LexError::UnexpectedChar),
            TokenType::InvalidIdent => TokenValue::Error(LexError::InvalidIdentifier),
            _ => TokenValue::None,
        };

        Token::new(token_type, start, end, token_value)
    }

    fn number(&mut self) -> TokenType {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // if number is not followed by whitespace or semicolon, consume until next whitespace and return an error
        if !self.peek().is_whitespace() && self.peek() != ';' {
            while self.peek().is_alphanumeric() {
                self.advance();
            }
            return TokenType::InvalidIdent;
        }

        TokenType::Constant
    }

    fn identifier(&mut self, start: usize) -> TokenType {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = self.source[start..self.offset()].to_string();

        match text.as_str() {
            "int" => TokenType::Int,
            "void" => TokenType::Void,
            "return" => TokenType::Return,
            _ => TokenType::Identifier,
        }
    }

    /// Get offset into source text
    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        Some(c)
    }
}
