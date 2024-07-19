use std::fmt::{Display, Formatter};

use thiserror::Error;

use crate::{Expr, Func, Stmt, Token, TokenType, TokenValue, TranslationUnit};

#[derive(Error, Clone, Debug)]
pub struct ParseError {
    message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl ParseError {
    fn new(message: String) -> Self {
        Self {
            message,
        }
    }
}

pub struct Parser {
    tokens: std::vec::IntoIter<Token>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter()
        }
    }

    pub fn parse(&mut self) -> Result<TranslationUnit, ParseError> {
        let func = self.parse_func()?;
        self.expect_empty()?;
        Ok(TranslationUnit {
            func
        })
    }

    fn parse_func(&mut self) -> Result<Func, ParseError> {
        self.expect(TokenType::Int)?;
        let name = self.parse_ident()?;

        self.expect(TokenType::OpenParen)?;
        self.expect(TokenType::Void)?;
        self.expect(TokenType::CloseParen)?;
        self.expect(TokenType::OpenBrace)?;

        let body = self.parse_stmt()?;

        self.expect(TokenType::CloseBrace)?;

        Ok(Func {
            ident: name,
            body,
        })
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        match self.tokens.next() {
            Some(Token {kind: TokenType::Identifier, value: TokenValue::Ident(ident), ..}) => Ok(ident.to_string()),
            Some(t) => Err(ParseError::new(format!("Expected an identifier, but found {:?}", t))),
            None => Err(ParseError::new("Expected an identifier, but found end of file instead".to_string()))
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenType::Return)?;

        let expr = self.parse_expr()?;

        self.expect(TokenType::Semicolon)?;

        Ok(Stmt {
            expr,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.tokens.next() {
            Some(Token {kind: TokenType::Constant, value: TokenValue::Integer(val), ..}) => Ok(Expr {
                val
            }),
            Some(t) => Err(ParseError::new(format!("Expected an expression, but found {:?}", t))),
            None => Err(ParseError::new("Expected an expression, but found end of file instead".to_string()))
        }
    }

    /// Checks if next token is of correct expected type
    fn expect(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        match self.tokens.next() {
            Some(t) if t.kind == expected => Ok(t),
            Some(t) => Err( ParseError::new(format!("Expected {:?}, but found {:?}", expected, t))),
            None => Err(ParseError::new(format!("Unexpected end of file"))),
        }
    }

    fn expect_empty(&mut self) -> Result<(), ParseError> {
        match self.tokens.next() {
            Some(t) => Err(ParseError::new(format!("Expected end of file, but found {:?}", t))),
            None => Ok(()),
        }
    }
}
