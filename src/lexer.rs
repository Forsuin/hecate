use thiserror::Error;

mod cursor;

pub struct Lexer {}

impl Lexer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn tokenize(&self, src: &str) -> Result<Vec<Token>, LexError> {
        Ok(vec![])
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub len: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Keywords will be considered Identifiers at this point
    Identifier,
    // Literals are just integers right now, will be changed later as more types are added
    Literal(i32),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("invalid character")]
    InvalidChar,
}
