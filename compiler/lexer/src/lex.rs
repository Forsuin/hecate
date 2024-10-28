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
    #[error("invalid suffix")]
    InvalidSuffix(String),
    #[error("number out of range")]
    OutOfRange(String),
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
    Long(i64),
    UnsignedInt(u32),
    UnsignedLong(u64),
    String(String),
    Ident(String),
    Error(LexError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
    Long,
    Signed,
    Unsigned,
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
    Error,
}

const EOF: char = '\0';

enum ConstType {
    Int,
    Long,
    UnsignedInt,
    UnsignedLong,
}

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
                }
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
                }
                '=' => {
                    self.advance();
                    TokenType::PlusEqual
                }
                _ => TokenType::Plus,
            },
            '*' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::StarEqual
                }
                _ => TokenType::Star,
            },
            '/' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::SlashEqual
                }
                _ => TokenType::Slash,
            },
            '%' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::PercentEqual
                }
                _ => TokenType::Percent,
            },
            '!' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::BangEqual
                }
                _ => TokenType::Bang,
            },
            '=' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::EqualEqual
                }
                _ => TokenType::Equal,
            },
            '&' => match self.peek() {
                '&' => {
                    self.advance();
                    TokenType::AmpAmp
                }
                '=' => {
                    self.advance();
                    TokenType::AmpEqual
                }
                _ => TokenType::Amp,
            },
            '|' => match self.peek() {
                '|' => {
                    self.advance();
                    TokenType::PipePipe
                }
                '=' => {
                    self.advance();
                    TokenType::PipeEqual
                }
                _ => TokenType::Pipe,
            },
            '^' => match self.peek() {
                '=' => {
                    self.advance();
                    TokenType::XorEqual
                }
                _ => TokenType::Xor,
            },
            '<' => match self.peek() {
                '<' => {
                    self.advance();

                    match self.peek() {
                        '=' => {
                            self.advance();
                            TokenType::LessLessEqual
                        }
                        _ => TokenType::LessLess,
                    }
                }
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
                        _ => TokenType::GreaterGreater,
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
                let val = self.convert_constant_value(&self.source[start..end]);

                match val {
                    Ok(val) => val,
                    Err(e) => {
                        return Token::new(
                            TokenType::Error,
                            start,
                            end,
                            TokenValue::Error(e),
                            self.line,
                            col,
                        )
                    }
                }
            }
            TokenType::Identifier => TokenValue::Ident(self.source[start..end].to_string()),
            TokenType::Unknown => TokenValue::Error(LexError::UnexpectedChar),
            TokenType::InvalidIdent => TokenValue::Error(LexError::InvalidIdentifier),
            _ => TokenValue::None,
        };

        Token::new(token_type, start, end, token_value, self.line, col)
    }

    fn convert_constant_value(&mut self, source: &str) -> Result<TokenValue, LexError> {
        let suffix = source.trim_start_matches(char::is_numeric);
        // get just prefix, if no suffix to strip, just use source
        let prefix = source
            .strip_suffix(suffix)
            .unwrap_or(source);

        // check if constant ends with suffix
        let const_type = match suffix {
            "" => ConstType::Int,
            suffix => self.check_valid_suffix(suffix)?,
        };

        Ok(match const_type {
            ConstType::Int => {
                // try to parse as an int, but automatically convert to long if too big
                match prefix.parse::<i32>() {
                    Ok(val) => TokenValue::Integer(val),
                    Err(_) => match prefix.parse::<i64>() {
                        Ok(val) => TokenValue::Long(val),
                        Err(_) => {
                            return Err(LexError::OutOfRange(source.to_string()));
                        }
                    },
                }
            }
            ConstType::Long => TokenValue::Long(prefix.parse::<i64>().unwrap()),
            ConstType::UnsignedInt => {
                // try to parse as an uint, but automatically convert to long if too big
                match prefix.parse::<u32>() {
                    Ok(val) => TokenValue::UnsignedInt(val),
                    Err(_) => match prefix.parse::<u64>() {
                        Ok(val) => TokenValue::UnsignedLong(val),
                        Err(_) => {
                            return Err(LexError::OutOfRange(source.to_string()));
                        }
                    },
                }
            }
            ConstType::UnsignedLong => {
                TokenValue::UnsignedLong(prefix.parse::<u64>().unwrap())
            }
        })
    }

    fn check_valid_suffix(&mut self, source: &str) -> Result<ConstType, LexError> {
        match source {
            "" => Ok(ConstType::Int),
            "l" | "L" | "ll" | "LL" => Ok(ConstType::Long),
            "u" | "U"  => Ok(ConstType::UnsignedInt),
            "ul" | "UL" | "lu" | "LU" => Ok(ConstType::UnsignedLong),
            suffix => Err(LexError::InvalidSuffix(suffix.to_string())),
        }
    }

    fn number(&mut self) -> TokenType {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // also consume suffix
        loop {
            match self.peek() {
                // suffixes are ok
                'l' | 'L' | 'u' | 'U' => {
                    self.advance();
                }
                // other identifier characters are not ok
                c if c.is_alphabetic() => return TokenType::Error,
                '_' => return TokenType::Error,

                // everything else is ok
                _ => break,
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
            "long" => TokenType::Long,
            "signed" => TokenType::Signed,
            "unsigned" => TokenType::Unsigned,
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
    use super::*;
    use crate::TokenType::*;

    #[test]
    fn minus_minus() {
        let src = "int main(void) { --5; }";

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens
                .filter(|t| t.kind == MinusMinus)
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
        let expected = vec![
            PlusEqual,
            MinusEqual,
            StarEqual,
            SlashEqual,
            PercentEqual,
            AmpEqual,
            PipeEqual,
            XorEqual,
            LessLessEqual,
            GreaterGreaterEqual,
        ];

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
        let expected = vec![
            Int, Identifier, OpenParen, Int, Identifier, Comma, Int, Identifier, CloseParen,
            Semicolon,
        ];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn static_kw() {
        let src = "static int x;";
        let expected = vec![Static, Int, Identifier, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn extern_kw() {
        let src = "extern int x;";
        let expected = vec![Extern, Int, Identifier, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn signed_kw() {
        let src = "signed int x;";
        let expected = vec![Signed, Int, Identifier, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn unsigned_kw() {
        let src = "unsigned int x;";
        let expected = vec![Unsigned, Int, Identifier, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn unsigned_int_literal() {
        let src = "unsigned int x = 5u;";
        let expected = vec![Unsigned, Int, Identifier, Equal, Constant, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn unsigned_long_literal() {
        let src = "unsigned long x = 5ul;";
        let expected = vec![Unsigned, Long, Identifier, Equal, Constant, Semicolon];

        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let tokens: Vec<_> = tokens.map(|t| t.kind).collect();

        assert_eq!(tokens, expected)
    }
}
