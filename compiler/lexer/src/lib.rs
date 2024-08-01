use std::str::Chars;

use thiserror::Error;

#[derive(Error, Clone, Debug, PartialEq, Eq)]
pub enum LexError {
    #[error("unexpected character")]
    UnexpectedChar,
    #[error("invalid identifier")]
    InvalidIdentifier,
    #[error("unterminated string")]
    UnterminatedString,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub start: usize,
    pub end: usize,
    pub value: TokenValue,
    pub line: i32,
    pub col: i32,
}

impl Token {
    fn new(
        kind: TokenType,
        start: usize,
        end: usize,
        value: TokenValue,
        line: i32,
        col: i32,
    ) -> Self {
        Self {
            kind,
            start,
            end,
            value,
            line,
            col,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenValue {
    None,
    Integer(i32),
    String(String),
    Ident(String),
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
    Tilde,
    Minus,
    MinusMinus,
    Plus,
    Asterisk,
    Slash,
    Percent,
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
    line: i32,
    col: i32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            line: 1,
            col: 1,
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
        }).filter(|t| t.kind != TokenType::Whitespace)
    }

    fn scan_token(&mut self) -> Token {
        let start = self.offset();
        let col = self.col;

        let c = match self.advance() {
            Some(c) => c,
            None => {
                return Token::new(
                    TokenType::Eof,
                    start,
                    self.offset(),
                    TokenValue::None,
                    self.line,
                    self.col,
                )
            }
        };

        let token_type = match c {
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            ';' => TokenType::Semicolon,
            '~' => TokenType::Tilde,
            '-' => {
                match self.peek() {
                    '-' => TokenType::MinusMinus,
                    _ => TokenType::Minus,
                }
            },
            '+' => TokenType::Plus,
            '*' => TokenType::Asterisk,
            '/' => TokenType::Slash,
            '%' => TokenType::Percent,
            _c @ '0'..='9' => self.number(),
            _c @ 'a'..='z' | _c @ 'A'..='Z' | _c @ '_' => self.identifier(start),
            ' ' | '\r' | '\t' => TokenType::Whitespace,
            '\n' => {
                self.line += 1;
                self.col = 1;
                TokenType::Whitespace
            }
            _ => TokenType::Unknown,
        };

        let end = self.offset();

        let token_value = match token_type {
            TokenType::Constant => {
                TokenValue::Integer(self.source[start..end].parse::<i32>().unwrap())
            }
            TokenType::Identifier => {
                TokenValue::Ident(self.source[start..end].to_string())
            }
            TokenType::Unknown => TokenValue::Error(LexError::UnexpectedChar),
            TokenType::InvalidIdent => TokenValue::Error(LexError::InvalidIdentifier),
            _ => TokenValue::None,
        };

        Token::new(token_type, start, end, token_value, self.line, col)
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
        self.col += 1;

        Some(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minus_minus() {
        let src = "int main(void) { --5; }";

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.filter(|t| t.kind == TokenType::MinusMinus).collect::<Vec<Token>>().len(), 1);
    }

    #[test]
    fn double_minus_paren() {
        let src = "int main(void) { -(-5); }";

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<Token> = tokens.collect();

        assert_eq!(tokens.iter().filter(|t| t.kind == TokenType::MinusMinus).collect::<Vec<_>>().len(), 0);
        assert_eq!(tokens.iter().filter(|t| t.kind == TokenType::Minus).collect::<Vec<_>>().len(), 2)
    }
}
