use ast::*;
use ty::*;

use crate::sem_err::*;

pub struct TypeChecker {
    // map of symbols in current scope
    pub symbols: Scope<Symbol>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbols: Scope::new(),
        }
    }

    pub fn check(&mut self, program: &TranslationUnit) -> SemanticResult<()> {
        for func in &program.funcs {
            self.check_func_decl(func)?
        }

        Ok(())
    }

    fn check_func_decl(&mut self, decl: &FuncDecl) -> SemanticResult<()> {
        let fun_type = FuncType {
            param_count: decl.params.len(),
        };
        let has_body = decl.body.is_some();
        let mut already_defined = false;

        if self.symbols.contains_key(&decl.ident) {
            let old_decl = self.symbols.get(&decl.ident).unwrap();
            let Type::Func(old_decl_func) = &old_decl.t else {
                return Err(SemErr::new(format!(
                    "Expected a function declaration, instead found {:?}",
                    old_decl
                )));
            };

            if old_decl_func.param_count != fun_type.param_count {
                return Err(SemErr::new(format!(
                    "Incompatible function declarations: {}",
                    decl.ident
                )));
            }

            already_defined = old_decl.init_type == InitType::Definition;
            if already_defined && has_body {
                return Err(SemErr::new(format!(
                    "Function is defined more than once: {}",
                    decl.ident
                )));
            }
        }

        self.symbols.insert(
            decl.ident.clone(),
            Symbol {
                t: Type::Func(FuncType {
                    param_count: decl.params.len(),
                }),
                init_type: if already_defined || has_body {
                    InitType::Definition
                } else {
                    InitType::Declaration
                },
                stack_frame_size: 0,
            },
        );

        if let Some(body) = &decl.body {
            for param in decl.params.clone() {
                self.symbols.insert(
                    param,
                    Symbol {
                        t: Type::Int,
                        init_type: InitType::Declaration,
                        stack_frame_size: 0,
                    },
                );
            }

            self.check_block(body)?
        }

        Ok(())
    }

    fn check_block(&mut self, block: &Block) -> SemanticResult<()> {
        for item in &block.items {
            self.check_block_item(item)?
        }

        Ok(())
    }

    fn check_block_item(&mut self, item: &BlockItem) -> SemanticResult<()> {
        match item {
            BlockItem::S(stmt) => {
                self.check_stmt(stmt)?;
            }
            BlockItem::D(decl) => {
                self.check_decl(decl)?;
            }
        }

        Ok(())
    }

    fn check_decl(&mut self, decl: &Decl) -> SemanticResult<()> {
        match decl {
            Decl::FuncDecl { func } => {
                self.check_func_decl(func)?;
            }
            Decl::VarDecl { var } => {
                self.check_var_decl(var)?;
            }
        }

        Ok(())
    }

    fn check_var_decl(&mut self, var: &VarDecl) -> SemanticResult<()> {
        self.symbols.insert(
            var.name.clone(),
            Symbol {
                t: Type::Int,
                init_type: InitType::Declaration,
                stack_frame_size: 0,
            },
        );

        if let Some(init) = &var.init {
            self.check_expr(init)?;
        }

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> SemanticResult<()> {
        match stmt {
            Stmt::Compound { block } => {
                self.check_block(block)?;
            }
            Stmt::Return { expr } => {
                self.check_expr(expr)?;
            }
            Stmt::Expression { expr } => {
                self.check_expr(expr)?;
            }
            Stmt::If {
                condition,
                then,
                otherwise,
            } => {
                self.check_expr(condition)?;
                self.check_stmt(then)?;

                if let Some(otherwise) = otherwise {
                    self.check_stmt(otherwise)?;
                }
            }
            Stmt::LabeledStmt { label: _, stmt } => {
                self.check_stmt(stmt)?;
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.check_expr(condition)?;
                self.check_stmt(body)?;
            }
            Stmt::DoWhile {
                body, condition, ..
            } => {
                self.check_stmt(body)?;
                self.check_expr(condition)?;
            }
            Stmt::For {
                init,
                condition,
                post,
                body,
                ..
            } => {
                match init {
                    ForInit::Decl(decl) => {
                        self.check_var_decl(decl)?;
                    }
                    ForInit::Expr(expr) => {
                        if let Some(expr) = expr {
                            self.check_expr(expr)?;
                        }
                    }
                }
                if let Some(condition) = condition {
                    self.check_expr(condition)?;
                }
                if let Some(post) = post {
                    self.check_expr(post)?;
                }

                self.check_stmt(body)?;
            }
            Stmt::Switch { control, body, .. } => {
                self.check_expr(control)?;
                self.check_stmt(body)?;
            }
            Stmt::Case { constant, body, .. } => {
                self.check_expr(constant)?;
                self.check_stmt(body)?;
            }
            Stmt::Default { body, .. } => {
                self.check_stmt(body)?;
            }
            Stmt::Null | Stmt::Goto { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => {}
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> SemanticResult<()> {
        match expr {
            Expr::Var(var) => {
                let var_type = &self.symbols.get(var).unwrap().t;

                match var_type {
                    Type::Int => {}
                    Type::Func(_) => {
                        return Err(SemErr::new(format!(
                            "Tried to use variable '{}' as a function",
                            var
                        )));
                    }
                }
            }
            Expr::Unary { op: _, expr } => {
                self.check_expr(expr)?;
            }
            Expr::Binary { op: _, left, right } => {
                self.check_expr(left)?;
                self.check_expr(right)?;
            }
            Expr::Assignment { lvalue, expr } => {
                self.check_expr(lvalue)?;
                self.check_expr(expr)?;
            }
            Expr::Conditional {
                condition,
                then,
                otherwise,
            } => {
                self.check_expr(condition)?;
                self.check_expr(then)?;
                self.check_expr(otherwise)?;
            }
            Expr::CompoundAssignment {
                op: _,
                lvalue,
                expr,
            } => {
                self.check_expr(lvalue)?;
                self.check_expr(expr)?;
            }
            Expr::PostfixInc(expr) => {
                self.check_expr(expr)?;
            }
            Expr::PostfixDec(expr) => {
                self.check_expr(expr)?;
            }
            Expr::FunctionCall { func, args } => {
                let func_type = &self.symbols.get(func).unwrap().t;

                match func_type {
                    Type::Int => {
                        return Err(SemErr::new(format!(
                            "Tried to use variable '{}' as function",
                            func
                        )));
                    }
                    Type::Func(ft) => {
                        if args.len() != ft.param_count {
                            return Err(SemErr::new(format!("Tried to call {} with incorrect number of arguments, expected {}, found {}", func, args.len(), ft.param_count)));
                        } else {
                            for arg in args {
                                self.check_expr(arg)?;
                            }
                        }
                    }
                }
            }
            Expr::Constant(_) => {}
        }

        Ok(())
    }
}
