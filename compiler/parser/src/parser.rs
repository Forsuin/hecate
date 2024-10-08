use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use thiserror::Error;

use ast::ExprKind::{Assignment, CompoundAssignment, Conditional};
use ast::*;
use lexer::*;
use ty::{Constant, FuncType, Type};

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

macro_rules! match_token_types {
    ($( $token:pat ),+ ) => {
        $(
        Some(Token{ kind: $token, ..})
        )|+
    };
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
        Ok(TranslationUnit {
            decls: self.parse_decl_list()?,
        })
    }

    fn parse_decl_list(&mut self) -> Result<Vec<Decl>, ParseError> {
        let mut decls = vec![];

        while let Some(_) = self.peek() {
            match self.parse_decl()? {
                Decl::FuncDecl(func) => {
                    decls.push(Decl::FuncDecl(func));
                }
                Decl::VarDecl(var) => decls.push(Decl::VarDecl(var)),
            }
        }

        self.expect_empty()?;

        Ok(decls)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(TokenType::OpenBrace)?;

        let mut body = vec![];

        while self
            .tokens
            .peek()
            .is_some_and(|t| t.kind != TokenType::CloseBrace)
        {
            let next_block = self.parse_block_item()?;
            body.push(next_block);
        }

        self.expect(TokenType::CloseBrace)?;

        Ok(Block { items: body })
    }

    fn parse_block_item(&mut self) -> Result<BlockItem, ParseError> {
        match self.tokens.peek() {
            match_token_types!(TokenType::Int, TokenType::Static, TokenType::Extern) => {
                Ok(BlockItem::D(self.parse_decl()?))
            }
            Some(_) => Ok(BlockItem::S(self.parse_stmt()?)),
            None => Err(ParseError::new(
                "Expected function body, but found end of file instead".to_string(),
            )),
        }
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        match self.parse_decl()? {
            Decl::FuncDecl(FuncDecl { ident, .. }) => Err(ParseError::new(format!(
                "Expected variable declaration but found function declaration: '{}'",
                ident
            ))),
            Decl::VarDecl(var) => Ok(var),
        }
    }

    fn parse_decl(&mut self) -> Result<Decl, ParseError> {
        let specifiers = self.parse_specifier_list()?;
        let (t, storage_class) = self.parse_type_and_storage(specifiers)?;

        let ident = self.parse_ident()?;

        match self.peek() {
            None => Err(ParseError::new(format!("Unexpected end of file"))),
            Some(Token {
                kind: TokenType::OpenParen,
                ..
            }) => Ok(Decl::FuncDecl(self.parse_rest_func_decl(
                ident,
                storage_class,
                t,
            )?)),
            Some(_) => Ok(Decl::VarDecl(self.parse_rest_var_decl(
                ident,
                storage_class,
                t,
            )?)),
        }
    }

    fn parse_specifier_list(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut specifiers = vec![];

        loop {
            match self.peek() {
                match_token_types!(
                    TokenType::Int,
                    TokenType::Long,
                    TokenType::Static,
                    TokenType::Extern
                ) => {
                    specifiers.push(self.tokens.next().unwrap());
                }
                _ => break,
            }
        }

        Ok(specifiers)
    }

    fn parse_type_and_storage(
        &mut self,
        specifiers: Vec<Token>,
    ) -> Result<(Type, Option<StorageClass>), ParseError> {
        let (types, storage_classes): (Vec<_>, Vec<_>) = specifiers
            .into_iter()
            .partition(|spec| matches!(spec.kind, TokenType::Int | TokenType::Long));

        if storage_classes.len() > 1 {
            return Err(ParseError::new(format!(
                "Expected one storage class, found multiple: '{:?}'",
                storage_classes
            )));
        }

        let ty = self.parse_type(types.iter().map(|t| t.kind).collect())?;
        let mut storage_class = None;

        if !storage_classes.is_empty() {
            storage_class = Some(self.parse_storage_class(&storage_classes[0])?);
        }

        Ok((ty, storage_class))
    }

    fn parse_storage_class(&mut self, storage_token: &Token) -> Result<StorageClass, ParseError> {
        match storage_token.kind {
            TokenType::Static => Ok(StorageClass::Static),
            TokenType::Extern => Ok(StorageClass::Extern),
            _ => Err(ParseError::new(format!(
                "Expected storage class but found '{:?}' instead",
                storage_token.kind
            ))),
        }
    }

    fn parse_type(&self, types: Vec<TokenType>) -> Result<Type, ParseError> {
        match types.as_slice() {
            [TokenType::Int] => Ok(Type::Int),
            [TokenType::Long]
            | [TokenType::Int, TokenType::Long]
            | [TokenType::Long, TokenType::Int] => Ok(Type::Long),
            [tokens @ ..] => {
                let invalid = tokens
                    .iter()
                    .map(|t| format!("{:?}", t))
                    .collect::<Vec<String>>()
                    .join(", ");

                Err(ParseError::new(format!(
                    "Invalid type specifier(s): '{:?}'",
                    invalid
                )))
            }
        }
    }

    fn parse_type_specifier(&mut self) -> Result<TokenType, ParseError> {
        let spec = self
            .tokens
            .next()
            .ok_or(ParseError::new("Unexpected EOF".to_string()))?;

        if is_type_specifier(spec.kind) {
            Ok(spec.kind)
        } else {
            Err(ParseError::new(format!(
                "Expected type specifier, found '{:?}'",
                spec
            )))
        }
    }

    fn parse_type_specifier_list(&mut self) -> Result<Vec<TokenType>, ParseError> {
        let mut specs = vec![];

        specs.push(self.parse_type_specifier()?);

        while let Some(token) = self.peek() {
            if is_type_specifier(token.kind) {
                specs.push(token.kind);
            } else {
                break;
            }
        }

        Ok(specs)
    }

    fn parse_param_list(&mut self) -> Result<Vec<(Type, String)>, ParseError> {
        if self.peek().unwrap().kind == TokenType::Void {
            Ok(vec![])
        } else {
            let mut params = vec![];

            loop {
                let type_spec_list = self.parse_type_specifier_list()?;
                let next_param_type = self.parse_type(type_spec_list)?;
                let next_param_name = self.parse_ident()?;
                params.push((next_param_type, next_param_name));

                if self.peek().unwrap().kind == TokenType::Comma {
                    self.expect(TokenType::Comma)?;
                } else {
                    break;
                }
            }

            Ok(params)
        }
    }

    fn parse_rest_func_decl(
        &mut self,
        name: String,
        storage_class: Option<StorageClass>,
        func_type: Type,
    ) -> Result<FuncDecl, ParseError> {
        self.expect(TokenType::OpenParen)?;

        let params_with_types = self.parse_param_list()?;
        let (param_types, param_names) = params_with_types.into_iter().unzip();
        let func_type = Type::Func(FuncType {
            param_types,
            return_type: Box::from(func_type.clone()),
        });

        let mut body = None;

        if self
            .peek()
            .is_some_and(|token| token.kind == TokenType::OpenBrace)
        {
            body = Some(self.parse_block()?);
        }
        // function declaration with no definition
        else {
            self.expect(TokenType::Semicolon)?;
        }

        Ok(FuncDecl {
            ident: name,
            params: param_names,
            body,
            storage_class,
            func_type,
        })
    }

    fn parse_rest_var_decl(
        &mut self,
        name: String,
        storage_class: Option<StorageClass>,
        var_type: Type,
    ) -> Result<VarDecl, ParseError> {
        let init;

        match self.tokens.peek() {
            Some(Token {
                kind: TokenType::Equal,
                ..
            }) => {
                self.expect(TokenType::Equal)?;
                init = Some(self.parse_expr(0)?);
                self.expect(TokenType::Semicolon)?;
            }
            Some(Token {
                kind: TokenType::Semicolon,
                ..
            }) => {
                self.expect(TokenType::Semicolon)?;
                init = None;
            }
            Some(t) => {
                return Err(ParseError::new(format!(
                    "Expected assignment or semicolon, found '{:?}' instead",
                    t
                )));
            }
            None => {
                return Err(ParseError::new(
                    "Expected assignment or semicolon, found end of file".to_string(),
                ))
            }
        }

        Ok(VarDecl {
            name,
            init,
            storage_class,
            var_type,
        })
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
        match self.peek_2() {
            (
                Some(Token {
                    kind: TokenType::Return,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Return)?;

                let expr = self.parse_expr(0)?;

                self.expect(TokenType::Semicolon)?;

                Ok(Stmt::Return { expr })
            }
            (
                Some(Token {
                    kind: TokenType::Semicolon,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Semicolon)?;
                Ok(Stmt::Null)
            }
            (
                Some(Token {
                    kind: TokenType::If,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::If)?;
                self.expect(TokenType::OpenParen)?;

                let condition = self.parse_expr(0)?;

                self.expect(TokenType::CloseParen)?;

                let then = self.parse_stmt()?;

                let otherwise = match self.peek() {
                    Some(Token {
                        kind: TokenType::Else,
                        ..
                    }) => {
                        self.expect(TokenType::Else)?;

                        Some(Box::from(self.parse_stmt()?))
                    }
                    _ => None,
                };

                Ok(Stmt::If {
                    condition,
                    then: Box::from(then),
                    otherwise,
                })
            }
            (
                Some(Token {
                    kind: TokenType::Goto,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Goto)?;
                let label = self.parse_ident()?;
                self.expect(TokenType::Semicolon)?;

                Ok(Stmt::Goto { label })
            }
            (
                Some(Token {
                    kind: TokenType::Identifier,
                    value: TokenValue::Ident(ident),
                    ..
                }),
                Some(Token {
                    kind: TokenType::Colon,
                    ..
                }),
            ) => {
                self.expect(TokenType::Identifier)?;
                self.expect(TokenType::Colon)?;

                Ok(Stmt::LabeledStmt {
                    label: ident,
                    stmt: Box::from(self.parse_stmt()?),
                })
            }
            (
                Some(Token {
                    kind: TokenType::OpenBrace,
                    ..
                }),
                _,
            ) => Ok(Stmt::Compound {
                block: self.parse_block()?,
            }),
            (
                Some(Token {
                    kind: TokenType::Break,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Break)?;
                self.expect(TokenType::Semicolon)?;
                Ok(Stmt::Break {
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::Continue,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Continue)?;
                self.expect(TokenType::Semicolon)?;
                Ok(Stmt::Continue {
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::While,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::While)?;
                self.expect(TokenType::OpenParen)?;

                let condition = self.parse_expr(0)?;

                self.expect(TokenType::CloseParen)?;

                let body = self.parse_stmt()?;

                Ok(Stmt::While {
                    condition,
                    body: Box::from(body),
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::Do,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Do)?;

                let body = self.parse_stmt()?;

                self.expect(TokenType::While)?;
                self.expect(TokenType::OpenParen)?;

                let condition = self.parse_expr(0)?;

                self.expect(TokenType::CloseParen)?;
                self.expect(TokenType::Semicolon)?;

                Ok(Stmt::DoWhile {
                    body: Box::from(body),
                    condition,
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::For,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::For)?;
                self.expect(TokenType::OpenParen)?;

                let init = self.parse_for_init()?;
                let condition = self.parse_optional_expr(TokenType::Semicolon)?;
                let post = self.parse_optional_expr(TokenType::CloseParen)?;
                let body = self.parse_stmt()?;

                Ok(Stmt::For {
                    init,
                    condition,
                    post,
                    body: Box::from(body),
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::Switch,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Switch)?;
                self.expect(TokenType::OpenParen)?;

                let control = self.parse_expr(0)?;

                self.expect(TokenType::CloseParen)?;

                let body = self.parse_stmt()?;

                Ok(Stmt::Switch {
                    control,
                    body: Box::from(body),
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::Case,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Case)?;

                let constant = self.parse_expr(0)?;

                self.expect(TokenType::Colon)?;

                let body = self.parse_stmt()?;

                Ok(Stmt::Case {
                    constant,
                    body: Box::from(body),
                    label: "".to_string(),
                })
            }
            (
                Some(Token {
                    kind: TokenType::Default,
                    ..
                }),
                _,
            ) => {
                self.expect(TokenType::Default)?;
                self.expect(TokenType::Colon)?;

                let body = self.parse_stmt()?;

                Ok(Stmt::Default {
                    body: Box::from(body),
                    label: "".to_string(),
                })
            }
            _ => {
                let expr = Stmt::Expression {
                    expr: self.parse_expr(0)?,
                };
                self.expect(TokenType::Semicolon)?;
                Ok(expr)
            }
        }
    }

    fn parse_for_init(&mut self) -> Result<ForInit, ParseError> {
        match self.peek() {
            match_token_types!(TokenType::Int, TokenType::Static, TokenType::Extern) => {
                Ok(ForInit::Decl(self.parse_var_decl()?))
            }
            Some(_) => Ok(ForInit::Expr(
                self.parse_optional_expr(TokenType::Semicolon)?,
            )),
            _ => Err(ParseError::new(format!(
                "Expect for-loop initializer, instead found {:#?}",
                self.peek()
            ))),
        }
    }

    fn parse_optional_expr(&mut self, delim: TokenType) -> Result<Option<Expr>, ParseError> {
        match self.peek() {
            Some(token) => {
                if token.kind == delim {
                    self.expect(delim)?;
                    Ok(None)
                } else {
                    let expr = self.parse_expr(0)?;
                    self.expect(delim)?;
                    Ok(Some(expr))
                }
            }
            None => Err(ParseError::new("Unexpected end of file".to_string())),
        }
    }

    fn parse_expr(&mut self, min_prec: i32) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;

        while let Some(next) = self.peek() {
            if let Some(prec) = get_precedence(next.kind) {
                if prec >= min_prec {
                    if is_assignment(next.kind) {
                        self.tokens.next();
                        let right = self.parse_expr(get_precedence(next.kind).unwrap())?;
                        left = match get_compound(next.kind) {
                            None => Expr::new(Assignment {
                                lvalue: Box::from(left),
                                expr: Box::from(right),
                            }),
                            Some(op) => Expr::new(CompoundAssignment {
                                op,
                                lvalue: Box::from(left),
                                expr: Box::from(right),
                            }),
                        }
                    } else if next.kind == TokenType::Question {
                        self.expect(TokenType::Question)?;
                        let middle = self.parse_expr(0)?;
                        self.expect(TokenType::Colon)?;

                        let right = self.parse_expr(get_precedence(next.kind).unwrap())?;

                        left = Expr::new(Conditional {
                            condition: Box::from(left),
                            then: Box::from(middle),
                            otherwise: Box::from(right),
                        })
                    } else {
                        let operator = self.parse_binop()?;
                        let right = self.parse_expr(prec + 1)?;
                        left = Expr::new(ExprKind::Binary {
                            op: operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        })
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
            match_token_types!(
                TokenType::Minus,
                TokenType::Tilde,
                TokenType::Bang,
                TokenType::MinusMinus,
                TokenType::PlusPlus
            ) => {
                let unop = self.parse_unop()?;
                let expr = self.parse_factor()?;

                Ok(Expr::new(ExprKind::Unary {
                    op: unop,
                    expr: Box::new(expr),
                }))
            }
            Some(_) => self.parse_postfix_expr(),
            None => Err(ParseError::new(
                "Expected an expression, but found end of file instead".to_string(),
            )),
        }
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let primary = self.parse_primary_expr()?;
        self.postfix_helper(primary)
    }

    fn postfix_helper(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        match self.tokens.peek() {
            Some(Token {
                kind: TokenType::MinusMinus,
                ..
            }) => {
                self.tokens.next();
                let dec_expr = Expr::new(ExprKind::PostfixDec(Box::from(expr)));
                self.postfix_helper(dec_expr)
            }
            Some(Token {
                kind: TokenType::PlusPlus,
                ..
            }) => {
                self.tokens.next();
                let dec_expr = Expr::new(ExprKind::PostfixInc(Box::from(expr)));
                self.postfix_helper(dec_expr)
            }
            _ => Ok(expr),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
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
                ..
            }) => {
                let value = self.parse_constant()?;
                Ok(Expr::new(ExprKind::Constant(value)))
            }
            Some(Token {
                kind: TokenType::Identifier,
                value: TokenValue::Ident(ident),
                ..
            }) => {
                let ident = ident.clone();
                self.expect(TokenType::Identifier)?;

                if let Some(Token {
                    kind: TokenType::OpenParen,
                    ..
                }) = self.peek()
                {
                    let args = self.parse_arg_list()?;

                    Ok(Expr::new(ExprKind::FunctionCall { func: ident, args }))
                } else {
                    Ok(Expr::new(ExprKind::Var(ident)))
                }
            }
            t => Err(ParseError::new(format!(
                "Expected a factor, found '{:?}'",
                t
            ))),
        }
    }

    fn parse_constant(&mut self) -> Result<Constant, ParseError> {
        let token = self.tokens.next();

        match token {
            None => Err(ParseError::new(
                "Unexpected end of file, expected constant".to_string(),
            )),
            Some(token) => match token {
                Token {
                    kind: TokenType::Constant,
                    value: TokenValue::Integer(value),
                    ..
                } => Ok(Constant::Int(value)),
                Token {
                    kind: TokenType::Constant,
                    value: TokenValue::Long(value),
                    ..
                } => Ok(Constant::Long(value)),
                _ => Err(ParseError::new(format!(
                    "Expected constant, found '{:?} at ({}, {})'",
                    token.kind, token.line, token.col
                ))),
            },
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.expect(TokenType::OpenParen)?;

        let mut args = vec![];

        loop {
            match self.peek() {
                None => {
                    return Err(ParseError::new(
                        "Expected function argument, found end of file instead".to_string(),
                    ))
                }
                Some(Token {
                    kind: TokenType::Comma,
                    ..
                }) => {
                    self.expect(TokenType::Comma)?;

                    if self
                        .peek()
                        .is_some_and(|token| token.kind == TokenType::CloseParen)
                    {
                        return Err(ParseError::new(
                            "Found trailing comma at end of argument list".to_string(),
                        ));
                    }
                }
                Some(Token {
                    kind: TokenType::CloseParen,
                    ..
                }) => {
                    self.expect(TokenType::CloseParen)?;
                    break;
                }
                Some(_) => {
                    args.push(self.parse_expr(0)?);
                }
            }
        }

        Ok(args)
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
            }) => Ok(UnaryOp::Dec),
            Some(Token {
                kind: TokenType::PlusPlus,
                ..
            }) => Ok(UnaryOp::Inc),
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
            None => Err(ParseError::new("Unexpected end of file".to_string())),
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

    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    fn peek_2(&mut self) -> (Option<Token>, Option<Token>) {
        let mut tokens = self.tokens.clone();
        let first = tokens.next();
        let second = tokens.next();

        (first, second)
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
        TokenType::Question => Some(3),
        TokenType::Equal
        | TokenType::PlusEqual
        | TokenType::MinusEqual
        | TokenType::StarEqual
        | TokenType::SlashEqual
        | TokenType::PercentEqual
        | TokenType::AmpEqual
        | TokenType::XorEqual
        | TokenType::PipeEqual
        | TokenType::LessLessEqual
        | TokenType::GreaterGreaterEqual => Some(1),
        _ => None,
    }
}

fn is_assignment(token_type: TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Equal
            | TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual
            | TokenType::PercentEqual
            | TokenType::AmpEqual
            | TokenType::XorEqual
            | TokenType::PipeEqual
            | TokenType::LessLessEqual
            | TokenType::GreaterGreaterEqual
    )
}

fn get_compound(token_type: TokenType) -> Option<BinaryOp> {
    match token_type {
        TokenType::Equal => None,

        TokenType::PlusEqual => Some(BinaryOp::Add),
        TokenType::MinusEqual => Some(BinaryOp::Subtract),
        TokenType::SlashEqual => Some(BinaryOp::Divide),
        TokenType::StarEqual => Some(BinaryOp::Multiply),
        TokenType::PercentEqual => Some(BinaryOp::Modulo),
        TokenType::AmpEqual => Some(BinaryOp::BitwiseAnd),
        TokenType::PipeEqual => Some(BinaryOp::BitwiseOr),
        TokenType::XorEqual => Some(BinaryOp::BitwiseXor),
        TokenType::LessLessEqual => Some(BinaryOp::BitshiftLeft),
        TokenType::GreaterGreaterEqual => Some(BinaryOp::BitshiftRight),

        _ => unreachable!("Not an assignment operator: '{:?}'", token_type),
    }
}

fn is_type_specifier(kind: TokenType) -> bool {
    match kind {
        TokenType::Int | TokenType::Long => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use ast::ExprKind::*;
    use lexer::*;
    use ty::Constant;

    use super::*;

    macro_rules! stmt_item {
        ($stmt:expr) => {
            BlockItem::S($stmt)
        };
    }

    macro_rules! case {
        ($constant:expr, $body:expr) => {
            Stmt::Case {
                constant: $constant,
                body: Box::new($body),
                label: "".to_string(),
            }
        };
    }

    // Creates a Constant::Int expression, prefer to use constant!()
    macro_rules! const_expr {
        ($expr:expr) => {
            constant!($expr, i32)
        };
    }

    macro_rules! constant {
        ($expr:expr, i32) => {
            Expr::new(ExprKind::Constant(Constant::Int($expr)))
        };
        ($expr:expr, i64) => {
            Expr::new(ExprKind::Constant(Constant::Long($expr)))
        };
    }

    macro_rules! break_stmt {
        () => {
            Stmt::Break {
                label: "".to_string(),
            }
        };
    }

    macro_rules! default {
        ($body:expr) => {
            Stmt::Default {
                body: Box::new($body),
                label: "".to_string(),
            }
        };
    }

    macro_rules! var {
        ($expr:literal) => {
            // String::from() so only string literals are accepted
            Expr::new(ExprKind::Var(String::from($expr)))
        };
    }

    /// Macro for generating a binary expression
    /// left and right expressions do NOT need to be boxed, the macro handles that
    macro_rules! binary {
        ($op:expr, $left:expr, $right:expr) => {
            Expr::new(ExprKind::Binary {
                op: $op,
                left: Box::new($left),
                right: Box::new($right),
            })
        };
    }

    /// Macro for generating a unary expression
    /// expr does NOT need to be boxed, the macro handles that
    macro_rules! unary {
        ($op:expr, $expr:expr) => {
            Expr::new(ExprKind::Unary {
                op: $op,
                expr: Box::new($expr),
            })
        };
    }

    /// Macro for generating a conditional expression
    /// condition, then, and otherwise do NOT need to be boxed, the macro handles that
    macro_rules! conditional {
        ($cond:expr, $then:expr, $otherwise:expr) => {
            Expr::new(ExprKind::Conditional {
                condition: Box::new($cond),
                then: Box::new($then),
                otherwise: Box::new($otherwise),
            })
        };
    }

    macro_rules! assignment {
        ($lvalue:expr, $expr:expr) => {
            Expr::new(ExprKind::Assignment {
                lvalue: Box::new($lvalue),
                expr: Box::new($expr),
            })
        };
    }

    macro_rules! compound_assignment {
        ($op:expr, $lvalue:expr, $expr:expr) => {
            Expr::new(ExprKind::CompoundAssignment {
                op: $op,
                lvalue: Box::new($lvalue),
                expr: Box::new($expr),
            })
        };
    }

    macro_rules! postfix_dec {
        ($expr:expr) => {
            Expr::new(PostfixDec(Box::new($expr)))
        };
    }

    macro_rules! postfix_inc {
        ($expr:expr) => {
            Expr::new(PostfixInc(Box::new($expr)))
        };
    }

    #[test]
    fn simple_add() {
        let src = "3 + 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(BinaryOp::Add, constant!(3, i32), constant!(5, i32))
        )
    }

    #[test]
    fn simple_sub() {
        let src = "3 - 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(BinaryOp::Subtract, constant!(3, i32), constant!(5, i32))
        )
    }

    #[test]
    fn simple_mul() {
        let src = "3 * 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(BinaryOp::Multiply, constant!(3, i32), constant!(5, i32))
        )
    }

    #[test]
    fn simple_div() {
        let src = "3 / 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(BinaryOp::Divide, constant!(3, i32), constant!(5, i32))
        )
    }

    #[test]
    fn simple_mod() {
        let src = "3 % 5";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(BinaryOp::Modulo, constant!(3, i32), constant!(5, i32))
        )
    }

    #[test]
    fn triple_add() {
        let src = "3 + 5 + 6";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(
                BinaryOp::Add,
                binary!(BinaryOp::Add, constant!(3, i32), constant!(5, i32)),
                constant!(6, i32)
            )
        )
    }

    #[test]
    fn add_mul() {
        let src = "3 + 5 + 6 * 2";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(
                BinaryOp::Add,
                binary!(BinaryOp::Add, constant!(3, i32), constant!(5, i32)),
                binary!(BinaryOp::Multiply, constant!(6, i32), constant!(2, i32))
            )
        )
    }

    #[test]
    fn add_with_unary() {
        let src = "3 + 5 + (-6)";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            binary!(
                BinaryOp::Add,
                binary!(BinaryOp::Add, constant!(3, i32), constant!(5, i32)),
                unary!(UnaryOp::Negate, constant!(6, i32))
            )
        )
    }

    #[test]
    fn if_stmt() {
        let src = "if (a == 0) return 5;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::If {
                condition: binary!(BinaryOp::Equal, var!("a"), constant!(0, i32)),
                then: Box::new(Stmt::Return {
                    expr: constant!(5, i32)
                }),
                otherwise: None,
            }
        )
    }

    #[test]
    fn if_else_stmt() {
        let src = "if (a == 0) return 5; else return 4;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::If {
                condition: binary!(BinaryOp::Equal, var!("a"), constant!(0, i32)),
                then: Box::new(Stmt::Return {
                    expr: constant!(5, i32)
                }),
                otherwise: Some(Box::new(Stmt::Return {
                    expr: constant!(4, i32)
                })),
            }
        )
    }

    #[test]
    fn nested_if_stmt() {
        let src = "if (a > 100) return 0; else if (a > 50) return 1; else return 2;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::If {
                condition: binary!(BinaryOp::Greater, var!("a"), constant!(100, i32)),
                then: Box::new(Stmt::Return {
                    expr: constant!(0, i32)
                }),
                otherwise: Some(Box::new(Stmt::If {
                    condition: binary!(BinaryOp::Greater, var!("a"), constant!(50, i32)),
                    then: Box::new(Stmt::Return {
                        expr: constant!(1, i32)
                    }),
                    otherwise: Some(Box::new(Stmt::Return {
                        expr: constant!(2, i32)
                    })),
                })),
            }
        )
    }

    #[test]
    fn ternary_expr() {
        let src = "a ? 1 : 0;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            conditional!(var!("a"), constant!(1, i32), constant!(0, i32))
        )
    }

    #[test]
    fn assign_ternary_expr() {
        let src = "a = 1 ? 2 : 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            assignment!(
                var!("a"),
                conditional!(constant!(1, i32), constant!(2, i32), constant!(3, i32))
            )
        )
    }

    #[test]
    fn ternary_expr_lower_precedence() {
        let src = "a = 1 ? 2 : 3 || 4;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(
            ast,
            assignment!(
                var!("a"),
                conditional!(
                    constant!(1, i32),
                    constant!(2, i32),
                    binary!(BinaryOp::Or, constant!(3, i32), constant!(4, i32))
                )
            )
        )
    }

    #[test]
    fn break_kw() {
        let src = "do break; while(1);";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::DoWhile {
                body: Box::new(Stmt::Break {
                    label: "".to_string()
                }),
                condition: constant!(1, i32),
                label: "".to_string(),
            }
        )
    }

    #[test]
    fn continue_kw() {
        let src = "do continue; while(1);";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::DoWhile {
                body: Box::new(Stmt::Continue {
                    label: "".to_string()
                }),
                condition: constant!(1, i32),
                label: "".to_string(),
            }
        )
    }

    #[test]
    fn while_stmt() {
        let src = "while(x > 0) x--; ";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::While {
                condition: binary!(BinaryOp::Greater, var!("x"), constant!(0, i32)),
                body: Box::new(Stmt::Expression {
                    expr: postfix_dec!(var!("x")),
                }),
                label: "".to_string(),
            }
        )
    }

    #[test]
    fn postfix_inc() {
        let src = "x++";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_expr(0).unwrap();

        assert_eq!(ast, postfix_inc!(var!("x")))
    }

    #[test]
    fn switch_stmt() {
        let src = "switch(x) { case 1: break; case 4: break; default: break; }";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::Switch {
                control: var!("x"),
                body: Box::new(Stmt::Compound {
                    block: Block {
                        items: vec![
                            stmt_item!(case!(const_expr!(1), break_stmt!())),
                            stmt_item!(case!(const_expr!(4), break_stmt!())),
                            stmt_item!(default!(break_stmt!()))
                        ]
                    }
                }),
                label: "".to_string()
            }
        )
    }

    #[test]
    fn static_var_decl() {
        let src = "static int a = 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(3, i32)),
                storage_class: Some(StorageClass::Static),
                var_type: Type::Int,
            })
        )
    }

    #[test]
    fn static_var_decl_reversed_specifiers() {
        let src = "int static a = 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(3, i32)),
                storage_class: Some(StorageClass::Static),
                var_type: Type::Int,
            })
        )
    }

    #[test]
    fn extern_var_decl() {
        let src = "extern int a = 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(3, i32)),
                storage_class: Some(StorageClass::Extern),
                var_type: Type::Int,
            })
        )
    }

    #[test]
    fn long_decl() {
        let src = "long a = 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(3, i32)),
                storage_class: None,
                var_type: Type::Long,
            })
        )
    }

    #[test]
    fn long_int_decl() {
        let src = "long int a = 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(3, i32)),
                storage_class: None,
                var_type: Type::Long,
            })
        )
    }

    #[test]
    fn int_long_decl() {
        let src = "int long a = 3;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(3, i32)),
                storage_class: None,
                var_type: Type::Long,
            })
        )
    }

    #[test]
    fn long_int_decl_with_long_constant() {
        let src = "long int a = 5000000000;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(5_000_000_000, i64)),
                storage_class: None,
                var_type: Type::Long,
            })
        )
    }

    #[test]
    fn static_int_with_long_init() {
        let src = "static int a = 100L;";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        println!("{:#?}", ast);

        assert_eq!(
            ast,
            Decl::VarDecl(VarDecl {
                name: "a".to_string(),
                init: Some(constant!(100, i64)),
                storage_class: Some(StorageClass::Static),
                var_type: Type::Int,
            })
        )
    }

    #[test]
    fn function_param_long() {
        let src = "int my_function(long a, long int b, int long c);";
        let tokens = Lexer::new(src).tokenize().collect();

        let ast = Parser::new(tokens).parse_decl().unwrap();

        println!("{:#?}", ast);

        assert_eq!(
            ast,
            Decl::FuncDecl(FuncDecl {
                ident: "my_function".to_string(),
                params: vec!["a", "b", "c"].iter().map(|x| x.to_string()).collect(),
                body: None,
                storage_class: None,
                func_type: Type::Func(ty::FuncType {
                    param_types: vec![Type::Long, Type::Long, Type::Long],
                    return_type: Box::new(Type::Int)
                })
            })
        )
    }
}
