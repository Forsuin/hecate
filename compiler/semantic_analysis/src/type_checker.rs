use ast::*;
use ty::*;

use crate::sem_err::*;

pub struct TypeChecker {
    // map of symbols in current scope
    pub symbols: SymbolTable,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
        }
    }

    pub fn check(&mut self, program: &TranslationUnit) -> SemanticResult<()> {
        for decl in &program.decls {
            match decl {
                Decl::FuncDecl(func) => self.check_func_decl(func)?,
                Decl::VarDecl(var) => self.check_file_scope_var_decl(var)?,
            }
        }

        Ok(())
    }

    fn check_file_scope_var_decl(&mut self, var: &VarDecl) -> SemanticResult<()> {
        let init = match var.init {
            Some(Expr::Constant(c)) => InitialVal::Initial(c),
            None => if var.storage_class == Some(StorageClass::Extern) { InitialVal::NoInit } else { InitialVal::Tentative },
            Some(_) => return Err(SemErr::new(format!("File scope variable '{}' has non-constant initializer", var.name)))
        };

        let global = var.storage_class != Some(StorageClass::Static);

        let old_decl = self.symbols.get(&var.name);

        let (global, init) = match old_decl {
            None => (global, init),
            Some(decl) => {
                if decl.t != Type::Int {
                    Err(SemErr::new(format!("Function '{}' redeclared as variable", var.name)))?;
                }

                match decl.attrs {
                    IdentifierAttr::Static { init: prev_init, global: prev_global } => {
                        let global = if var.storage_class == Some(StorageClass::Extern) { prev_global } else if global == prev_global { global } else { return Err(SemErr::new(format!("Conflicting variable linkage for: '{}'", var.name))); };
                        
                        let init = match (prev_init, init) {
                            (InitialVal::Initial(_), InitialVal::Initial(_)) => {
                                return Err(SemErr::new(format!("Conflicting global variable definition: {}", var.name)));
                            },
                            (InitialVal::Initial(_), _) => init,
                            (InitialVal::Tentative, InitialVal::Tentative | InitialVal::NoInit) => InitialVal::Tentative,
                            (_, InitialVal::Initial(_)) | (InitialVal::NoInit, _) => init
                        };

                        (global, init)
                    },
                    _ => return Err(SemErr::new(format!("Internal Error, file-scope variable previously declared as variable or function: {}", var.name))),
                }
            }
        };

        self.symbols.add_static_var(var.name.clone(), Type::Int, global, init);

        Ok(())
    }

    fn check_func_decl(&mut self, decl: &FuncDecl) -> SemanticResult<()> {
        let fun_type = FuncType {
            param_count: decl.params.len(),
        };
        let has_body = decl.body.is_some();
        let mut defined = false;
        let mut global = decl.storage_class != Some(StorageClass::Static);

        if self.symbols.is_defined(&decl.ident) {
            let old_decl = self.symbols.get(&decl.ident).unwrap();

            if old_decl.t != Type::Func(fun_type.clone()) {
                return Err(SemErr::new(format!("Redeclared '{}, Type: {:?}' with a different type: '{}'", decl.ident, old_decl.t, decl.params.len())));
            }

            match old_decl.attrs {
                IdentifierAttr::Func { defined: already_defined, global: already_global, .. } => {
                    if already_defined && has_body {
                        return Err(SemErr::new(format!("Defined '{}' with body multiple times", decl.ident)));
                    }
                    else if already_global && decl.storage_class == Some(StorageClass::Static) {
                        return Err(SemErr::new(format!("Static declaration of '{}' follow non-static declaration", decl.ident)));
                    }
                    else {
                        defined = already_defined || has_body;
                        global = already_global;
                    }
                }
                _ => return Err(SemErr::new(format!("Symbol '{}' has function type, but not function attributes", decl.ident)))
            }
        }
        else {
            defined = has_body;
        }

        self.symbols.add_func(decl.ident.clone(), Type::Func(fun_type), global, defined);

        if let Some(body) = &decl.body {
            for param in decl.params.clone() {
                self.symbols.add_automatic_var(param, Type::Int);
            }

            self.check_block(body)?;
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
                self.check_local_decl(decl)?;
            }
        }

        Ok(())
    }

    fn check_local_decl(&mut self, decl: &Decl) -> SemanticResult<()> {
        match decl {
            Decl::FuncDecl(func)=> {
                self.check_func_decl(func)?;
            }
            Decl::VarDecl(var) => {
                self.check_local_var_decl(var)?;
            }
        }

        Ok(())
    }

    fn check_local_var_decl(&mut self, var: &VarDecl) -> SemanticResult<()> {
        match var.storage_class {
            Some(StorageClass::Extern) => {
                if var.init.is_some() {
                    return Err(SemErr::new(format!("Initializer on local extern variable declaration: '{}'", var.name)));
                }

                match self.symbols.get(&var.name) {
                    None => {
                        self.symbols.add_static_var(var.name.clone(), Type::Int, true, InitialVal::NoInit);
                    }
                    Some(old_decl) => {
                        if old_decl.t != Type::Int {
                            return Err(SemErr::new(format!("Function '{}' redeclared as variable", var.name)));
                        }
                    }
                }
            },
            Some(StorageClass::Static) => {
                let init_val = match var.init {
                    Some(Expr::Constant(i)) => InitialVal::Initial(i),
                    None => InitialVal::Initial(0),
                    Some(_) => return Err(SemErr::new(format!("Non-constant initializer on local static variable: '{}'", var.name)))
                };

                self.symbols.add_static_var(var.name.clone(), Type::Int, false, init_val);
            },
            None => {
                self.symbols.add_automatic_var(var.name.clone(), Type::Int);
                match &var.init {
                    Some(init) => self.check_expr(init)?,
                    None => {}
                }
            }
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
                    ForInit::Decl(VarDecl {
                                      storage_class: Some(_),
                                      name,
                                      ..
                                  }) => {
                        return Err(SemErr::new(format!("Storage class not permitted on variable declaration in for-loop: '{}'", name)));
                    }
                    ForInit::Decl(decl) => {
                        self.check_local_var_decl(decl)?;
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
                            "Tried to use function '{}' as a variable",
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
