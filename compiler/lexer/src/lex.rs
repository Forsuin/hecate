
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

#[derive(Debug, Clone)]
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

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }

    fn ne(&self, other: &Self) -> bool {
        self.kind != other.kind
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
    // Single-character tokens
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Tilde,
    Question,
    Colon,
    Comma,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Minus,
    MinusEqual,
    MinusMinus,
    Plus,
    PlusPlus,
    PlusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Amp,
    AmpEqual,
    AmpAmp,
    Pipe,
    PipeEqual,
    PipePipe,
    Less,
    LessEqual,
    LessLess,
    LessLessEqual,
    Greater,
    GreaterEqual,
    GreaterGreater,
    GreaterGreaterEqual,
    Xor,
    XorEqual,

    // Literals
    Identifier,
    Constant,

    // Keywords
    Int,
    If,
    Else,
    Void,
    Return,
    Goto,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Static,
    Extern,

    // Informational
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
        })
            .filter(|t| t.kind != TokenType::Whitespace)
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
            ':' => TokenType::Colon,
            '?' => TokenType::Question,
            '~' => TokenType::Tilde,
            ',' => TokenType::Comma,
            '-' => match self.peek() {
                '-' => {
                    self.advance();
                    TokenType::MinusMinus
                },
                '=' => {
                    self.advance();
                    TokenType::MinusEqual
                }
                _ => TokenType::Minus,
            },
            '+' => match self.peek() {
                '+' => {
                    self.advance();
                    TokenType::PlusPlus
                },
                '=' => {
                    self.advance();
                    TokenType::PlusEqual
                }
                _ => TokenType::Plus
            },
            '*' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::StarEqual
                }
                _ => TokenType::Star
            },
            '/' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::SlashEqual
                }
                _ => TokenType::Slash
            },
            '%' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::PercentEqual
                }
                _ => TokenType::Percent
            },
            '!' => {
                match self.peek() {
                    '=' => {
                        self.advance();
                        TokenType::BangEqual
                    }
                    _ => TokenType::Bang
                }
            },
            '=' => {
                match self.peek() {
                    '=' => {
                        self.advance();
                        TokenType::EqualEqual
                    }
                    _ => TokenType::Equal
                }
            }
            '&' => {
                match self.peek() {
                    '&' => {
                        self.advance();
                        TokenType::AmpAmp
                    }
                    '=' => {
                        self.advance();
                        TokenType::AmpEqual
                    }
                    _ => TokenType::Amp
                }
            },
            '|' => {
                match self.peek() {
                    '|' => {
                        self.advance();
                        TokenType::PipePipe
                    }
                    '=' => {
                        self.advance();
                        TokenType::PipeEqual
                    }
                    _ => TokenType::Pipe
                }
            },
            '^' => {
                match self.peek() {
                    '=' => {
                        self.advance();
                        TokenType::XorEqual
                    }
                    _ => TokenType::Xor
                }
            },
            '<' => match self.peek() {
                '<' => {
                    self.advance();

                    match self.peek() {
                        '=' => {
                            self.advance();
                            TokenType::LessLessEqual
                        }
                        _ => TokenType::LessLess
                    }
                },
                '=' => {
                    self.advance();
                    TokenType::LessEqual
                }
                _ => TokenType::Less,
            },
            '>' => match self.peek() {
                '>' => {
                    self.advance();

                    match self.peek() {
                        '=' => {
                            self.advance();
                            TokenType::GreaterGreaterEqual
                        }
                        _ => TokenType::GreaterGreater
                    }
                }
                '=' => {
                    self.advance();
                    TokenType::GreaterEqual
                }
                _ => TokenType::Greater,
            },
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
            TokenType::Identifier => TokenValue::Ident(self.source[start..end].to_string()),
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
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = self.source[start..self.offset()].to_string();

        match text.as_str() {
            "int" => TokenType::Int,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "void" => TokenType::Void,
            "return" => TokenType::Return,
            "goto" => TokenType::Goto,
            "do" => TokenType::Do,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "switch" => TokenType::Switch,
            "case" => TokenType::Case,
            "default" => TokenType::Default,
            "static" => TokenType::Static,
            "extern" => TokenType::Extern,
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
    use crate::TokenType::*;
    use super::*;

    #[test]
    fn minus_minus() {
        let src = "int main(void) { --5; }";

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens
                .filter(|t| t.kind == TokenType::MinusMinus)
                .collect::<Vec<Token>>()
                .len(),
            1
        );
    }

    #[test]
    fn double_minus_paren() {
        let src = "int main(void) { -(-5); }";

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<Token> = tokens.collect();

        assert_eq!(
            tokens
                .iter()
                .filter(|t| t.kind == TokenType::MinusMinus)
                .collect::<Vec<_>>()
                .len(),
            0
        );
        assert_eq!(
            tokens
                .iter()
                .filter(|t| t.kind == TokenType::Minus)
                .collect::<Vec<_>>()
                .len(),
            2
        )
    }

    #[test]
    fn compound_assignment() {
        let src = "+= -= *= /= %= &= |= ^= <<= >>=";
        let expected = vec![PlusEqual, MinusEqual, StarEqual, SlashEqual, PercentEqual, AmpEqual, PipeEqual, XorEqual, LessLessEqual, GreaterGreaterEqual];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn inc_dec_ops() {
        let src = "++ --";
        let expected = vec![PlusPlus, MinusMinus];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn if_else() {
        let src = "if else";
        let expected = vec![If, Else];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn conditional_expr() {
        let src = "? :";
        let expected = vec![Question, Colon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn goto() {
        let src = "goto label;";
        let expected = vec![Goto, Identifier, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn label() {
        let src = "label: return 1;";
        let expected = vec![Identifier, Colon, Return, Constant, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn loop_keywords() {
        let src = "do while for break continue";
        let expected = vec![Do, While, For, Break, Continue];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn switch_case_keywords() {
        let src = "switch case default";
        let expected = vec![Switch, Case, Default];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn comma_char() {
        let src = ",";
        let expected = vec![Comma];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn comma_func_decl() {
        let src = "int func(int first, int second);";
        let expected = vec![Int, Identifier, OpenParen, Int, Identifier, Comma, Int, Identifier, CloseParen, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }
}
