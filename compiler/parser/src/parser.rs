use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use thiserror::Error;

use ast::*;
use lexer::*;

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
        Self { message }
    }
}

pub struct Parser {
    tokens: Peekable<std::vec::IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<TranslationUnit, ParseError> {
        let func = self.parse_func()?;
        self.expect_empty()?;
        Ok(TranslationUnit { func })
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

        Ok(Func { ident: name, body })
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        match self.tokens.next() {
            Some(Token {
                kind: TokenType::Identifier,
                value: TokenValue::Ident(ident),
                ..
            }) => Ok(ident.to_string()),
            Some(t) => Err(ParseError::new(format!(
                "Expected an identifier, but found {:?}",
                t
            ))),
            None => Err(ParseError::new(
                "Expected an identifier, but found end of file instead".to_string(),
            )),
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenType::Return)?;

        let expr = self.parse_expr(0)?;

        self.expect(TokenType::Semicolon)?;

        Ok(Stmt::Return { expr })
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;

        while let Some(next) = self.tokens.peek() {
            if let Some(prec) = get_precedence(next.kind) {
                if prec >= min_prec {
                    let operator = self.parse_binop()?;
                    let right = self.parse_expr(prec + 1)?;
                    left = Expr::Binary {
                        op: operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        match self.tokens.peek() {
            Some(Token {
                kind: TokenType::OpenParen,
                ..
            }) => {
                self.tokens.next();
                let expr = self.parse_expr(0)?;
                self.expect(TokenType::CloseParen)?;

                Ok(expr)
            }
            Some(Token {
                kind: TokenType::Constant,
                value: TokenValue::Integer(val),
                ..
            }) => {
                let val = val.clone();
                self.tokens.next();
                Ok(Expr::Constant(val))
            }
            Some(_) => {
                let unop = self.parse_unop()?;
                let expr = self.parse_factor()?;

                Ok(Expr::Unary {
                    op: unop,
                    expr: Box::new(expr),
                })
            }
            None => Err(ParseError::new(
                "Expected an expression, but found end of file instead".to_string(),
            )),
        }
    }

    fn parse_unop(&mut self) -> Result<UnaryOp, ParseError> {
        match self.tokens.next() {
            Some(Token {
                kind: TokenType::Minus,
                ..
            }) => Ok(UnaryOp::Negate),
            Some(Token {
                kind: TokenType::Tilde,
                ..
            }) => Ok(UnaryOp::Complement),
            Some(Token {
                kind: TokenType::Bang,
                ..
            }) => Ok(UnaryOp::Not),
            Some(Token {
                kind: TokenType::MinusMinus,
                ..
            }) => Err(ParseError::new(format!("Invalid operator '--'"))),
            Some(t) => Err(ParseError::new(format!(
                "Expected unary operator, found '{:?}'",
                t
            ))),
            None => Err(ParseError::new(format!("Unexpected end of file"))),
        }
    }

    fn parse_binop(&mut self) -> Result<BinaryOp, ParseError> {
        let t = self.tokens.next();

        match t {
            Some(Token {
                kind: TokenType::Plus,
                ..
            }) => Ok(BinaryOp::Add),
            Some(Token {
                kind: TokenType::Minus,
                ..
            }) => Ok(BinaryOp::Subtract),
            Some(Token {
                kind: TokenType::Star,
                ..
            }) => Ok(BinaryOp::Multiply),
            Some(Token {
                kind: TokenType::Slash,
                ..
            }) => Ok(BinaryOp::Divide),
            Some(Token {
                kind: TokenType::Percent,
                ..
            }) => Ok(BinaryOp::Modulo),

            Some(Token {
                kind: TokenType::Less,
                ..
            }) => Ok(BinaryOp::Less),
            Some(Token {
                kind: TokenType::LessEqual,
                ..
            }) => Ok(BinaryOp::LessEqual),
            Some(Token {
                kind: TokenType::Greater,
                ..
            }) => Ok(BinaryOp::Greater),
            Some(Token {
                kind: TokenType::GreaterEqual,
                ..
            }) => Ok(BinaryOp::GreaterEqual),
            Some(Token {
                kind: TokenType::AmpAmp,
                ..
            }) => Ok(BinaryOp::And),
            Some(Token {
                kind: TokenType::PipePipe,
                ..
            }) => Ok(BinaryOp::Or),
            Some(Token {
                kind: TokenType::EqualEqual,
                ..
            }) => Ok(BinaryOp::Equal),
            Some(Token {
                kind: TokenType::BangEqual,
                ..
            }) => Ok(BinaryOp::NotEqual),

            // Bitwise
            Some(Token {
                kind: TokenType::Amp,
                ..
            }) => Ok(BinaryOp::BitwiseAnd),
            Some(Token {
                kind: TokenType::Pipe,
                ..
            }) => Ok(BinaryOp::BitwiseOr),
            Some(Token {
                kind: TokenType::Xor,
                ..
            }) => Ok(BinaryOp::BitwiseXor),
            Some(Token {
                kind: TokenType::LessLess,
                ..
            }) => Ok(BinaryOp::BitshiftLeft),
            Some(Token {
                kind: TokenType::GreaterGreater,
                ..
            }) => Ok(BinaryOp::BitshiftRight),
            _ => Err(ParseError::new(format!(
                "Expected binary operator, found {:?}",
                t
            ))),
        }
    }

    /// Checks if next token is of correct expected type
    fn expect(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        match self.tokens.next() {
            Some(t) if t.kind == expected => Ok(t),
            Some(t) => Err(ParseError::new(format!(
                "Expected {:?}, but found {:?}",
                expected, t
            ))),
            None => Err(ParseError::new(format!("Unexpected end of file"))),
        }
    }

    fn expect_empty(&mut self) -> Result<(), ParseError> {
        match self.tokens.next() {
            Some(t) => Err(ParseError::new(format!(
                "Expected end of file, but found {:?}",
                t
            ))),
            None => Ok(()),
        }
    }
}

fn get_precedence(token: TokenType) -> Option<i32> {
    match token {
        TokenType::Star | TokenType::Slash | TokenType::Percent => Some(50),
        TokenType::Plus | TokenType::Minus => Some(45),
        TokenType::LessLess | TokenType::GreaterGreater => Some(40),
        TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual => {
            Some(35)
        }
        TokenType::EqualEqual | TokenType::BangEqual => Some(30),
        TokenType::Amp => Some(25),
        TokenType::Xor => Some(20),
        TokenType::Pipe => Some(15),
        TokenType::AmpAmp => Some(10),
        TokenType::PipePipe => Some(5),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use lexer::*;

    use super::*;

    #[test]
    fn simple_add() {
        let src = "3 + 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Constant(3)),
                right: Box::new(Expr::Constant(5)),
            }
        )
    }

    #[test]
    fn simple_sub() {
        let src = "3 - 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Subtract,
                left: Box::new(Expr::Constant(3)),
                right: Box::new(Expr::Constant(5)),
            }
        )
    }

    #[test]
    fn simple_mul() {
        let src = "3 * 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Multiply,
                left: Box::new(Expr::Constant(3)),
                right: Box::new(Expr::Constant(5)),
            }
        )
    }

    #[test]
    fn simple_div() {
        let src = "3 / 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Divide,
                left: Box::new(Expr::Constant(3)),
                right: Box::new(Expr::Constant(5)),
            }
        )
    }

    #[test]
    fn simple_mod() {
        let src = "3 % 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Modulo,
                left: Box::new(Expr::Constant(3)),
                right: Box::new(Expr::Constant(5)),
            }
        )
    }

    #[test]
    fn triple_add() {
        let src = "3 + 5 + 6";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Constant(3)),
                    right: Box::new(Expr::Constant(5)),
                }),
                right: Box::new(Expr::Constant(6)),
            }
        )
    }

    #[test]
    fn add_mul() {
        let src = "3 + 5 + 6 * 2";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Constant(3)),
                    right: Box::new(Expr::Constant(5)),
                }),
                right: Box::new(Expr::Binary {
                    op: BinaryOp::Multiply,
                    left: Box::new(Expr::Constant(6)),
                    right: Box::new(Expr::Constant(2)),
                }),
            }
        )
    }

    #[test]
    fn add_with_unary() {
        let src = "3 + 5 + (-6)";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Constant(3)),
                    right: Box::new(Expr::Constant(5)),
                }),
                right: Box::new(Expr::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(Expr::Constant(6))
                }),
            }
        )
    }
}
