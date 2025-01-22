use ast::*;
use std::iter::zip;
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

    pub fn check(&mut self, program: &mut TranslationUnit) -> SemanticResult<()> {
        for decl in &mut program.decls {
            match decl {
                Decl::FuncDecl(func) => self.check_func_decl(func)?,
                Decl::VarDecl(var) => self.check_file_scope_var_decl(var)?,
            }
        }

        Ok(())
    }

    fn check_file_scope_var_decl(&mut self, var: &VarDecl) -> SemanticResult<()> {
        let default_init = if var.storage_class == Some(StorageClass::Extern) {
            InitialVal::NoInit
        } else {
            InitialVal::Tentative
        };

        let current_init = match &var.init {
            Some(init) => to_static_init(var.var_type.clone(), init.clone())?,
            None => default_init,
        };

        let current_global = var.storage_class != Some(StorageClass::Static);

        let old_decl = self.symbols.get(&var.name);

        let (global, init) = match old_decl {
            None => (current_global, current_init.clone()),
            Some(decl) => {
                if matches!(decl.t, Type::Func(..)) {
                    Err(SemErr::new(format!(
                        "Function '{}' redeclared as variable",
                        var.name
                    )))?;
                }

                if decl.t != var.var_type {
                    return Err(SemErr::new(format!(
                        "Variable '{}' redeclared with a different type: '{:?}'",
                        var.name, var.var_type
                    )));
                }

                match &decl.attrs {
                    IdentifierAttr::Static { init: prev_init, global: prev_global } => {
                        let global = if var.storage_class == Some(StorageClass::Extern) {
                            *prev_global
                        } else if current_global == *prev_global {
                            current_global
                        } else {
                            return Err(SemErr::new(format!("Conflicting variable linkage for: '{}'", var.name)));
                        };

                        let init = match (prev_init, current_init.clone()) {
                            (InitialVal::Initial(_), InitialVal::Initial(_)) => {
                                return Err(SemErr::new(format!("Conflicting global variable definition: {}", var.name)));
                            },
                            (InitialVal::Initial(_), _) => prev_init.clone(),
                            (InitialVal::Tentative, InitialVal::Tentative | InitialVal::NoInit) => InitialVal::Tentative,
                            (_, InitialVal::Initial(_)) | (InitialVal::NoInit, _) => current_init.clone()
                        };

                        (global, init)
                    },
                    _ => return Err(SemErr::new(format!("Internal Error, file-scope variable previously declared as variable or function: {}", var.name))),
                }
            }
        };

        self.symbols
            .add_static_var(var.name.clone(), var.var_type.clone(), global, init);

        Ok(())
    }

    fn check_func_decl(&mut self, decl: &mut FuncDecl) -> SemanticResult<()> {
        let func_type = decl.func_type.clone();

        let has_body = decl.body.is_some();
        let defined;
        let mut global = decl.storage_class != Some(StorageClass::Static);

        if self.symbols.is_defined(&decl.ident) {
            let old_decl = self.symbols.get(&decl.ident).unwrap();

            if old_decl.t != func_type {
                return Err(SemErr::new(format!(
                    "Redeclared '{}, Type: {:?}' with a different type: '{}'",
                    decl.ident,
                    old_decl.t,
                    decl.params.len()
                )));
            }

            match old_decl.attrs {
                IdentifierAttr::Func {
                    defined: already_defined,
                    global: already_global,
                    ..
                } => {
                    if already_defined && has_body {
                        return Err(SemErr::new(format!(
                            "Defined '{}' with body multiple times",
                            decl.ident
                        )));
                    } else if already_global && decl.storage_class == Some(StorageClass::Static) {
                        return Err(SemErr::new(format!(
                            "Static declaration of '{}' follow non-static declaration",
                            decl.ident
                        )));
                    } else {
                        defined = already_defined || has_body;
                        global = already_global;
                    }
                }
                _ => {
                    return Err(SemErr::new(format!(
                        "Symbol '{}' has function type, but not function attributes",
                        decl.ident
                    )))
                }
            }
        } else {
            defined = has_body;
        }

        self.symbols
            .add_func(decl.ident.clone(), func_type.clone(), global, defined);

        if let Some(body) = &mut decl.body {
            let (param_types, return_type) = match func_type {
                Type::Func(FuncType {
                    param_types,
                    return_type,
                }) => (param_types, return_type),
                _ => {
                    return Err(SemErr::new(format!(
                        "Internal Error: function '{}' has non-function type",
                        decl.ident
                    )));
                }
            };

            for (param_name, param_type) in zip(decl.params.clone(), param_types) {
                self.symbols.add_automatic_var(param_name, param_type);
            }

            self.check_block(body, *return_type)?;
        }

        Ok(())
    }

    fn check_block(&mut self, block: &mut Block, return_type: Type) -> SemanticResult<()> {
        for item in &mut block.items {
            self.check_block_item(item, return_type.clone())?
        }

        Ok(())
    }

    fn check_block_item(&mut self, item: &mut BlockItem, return_type: Type) -> SemanticResult<()> {
        match item {
            BlockItem::S(stmt) => {
                self.check_stmt(stmt, return_type)?;
            }
            BlockItem::D(decl) => {
                self.check_local_decl(decl)?;
            }
        }

        Ok(())
    }

    fn check_local_decl(&mut self, decl: &mut Decl) -> SemanticResult<()> {
        match decl {
            Decl::FuncDecl(func) => {
                self.check_func_decl(func)?;
            }
            Decl::VarDecl(var) => {
                self.check_local_var_decl(var)?;
            }
        }

        Ok(())
    }

    fn check_local_var_decl(&mut self, var: &mut VarDecl) -> SemanticResult<()> {
        match var.storage_class {
            Some(StorageClass::Extern) => {
                if var.init.is_some() {
                    return Err(SemErr::new(format!(
                        "Initializer on local extern variable declaration: '{}'",
                        var.name
                    )));
                }

                match self.symbols.get(&var.name) {
                    None => {
                        self.symbols.add_static_var(
                            var.name.clone(),
                            Type::Int,
                            true,
                            InitialVal::NoInit,
                        );
                    }
                    Some(old_decl) => {
                        if old_decl.t != var.var_type {
                            return Err(SemErr::new(format!(
                                "{} '{}' redeclared with different type: {}",
                                old_decl.t, var.name, var.var_type
                            )));
                        }
                    }
                }
            }
            Some(StorageClass::Static) => {
                let zero_init = InitialVal::Initial(match &var.var_type {
                    Type::Int => StaticInit::Int(0),
                    Type::Long => StaticInit::Long(0),
                    Type::UInt => StaticInit::UInt(0),
                    Type::ULong => StaticInit::ULong(0),
                    Type::Float => StaticInit::Float(0.0),
                    Type::Double => StaticInit::Double(0.0),
                    Type::Func(_) => {
                        return Err(SemErr::new(format!(
                            "Internal Error: Attempted to static init function '{}'",
                            var.name
                        )));
                    }
                });

                let static_init = match &var.init {
                    Some(init) => to_static_init(var.var_type.clone(), init.clone())?,
                    None => zero_init,
                };

                self.symbols
                    .add_static_var(var.name.clone(), Type::Int, false, static_init);
            }
            None => {
                self.symbols
                    .add_automatic_var(var.name.clone(), var.var_type.clone());

                if let Some(init) = &mut var.init {
                    self.check_expr(init)?;

                    let init_cast = convert_to(init, var.var_type.clone());

                    *init = init_cast;
                }
            }
        }

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &mut Stmt, return_type: Type) -> SemanticResult<()> {
        match stmt {
            Stmt::Compound { block } => {
                self.check_block(block, return_type)?;
            }
            Stmt::Return { expr } => {
                self.check_expr(expr)?;

                *expr = convert_to(expr, return_type)
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
                self.check_stmt(then, return_type.clone())?;

                if let Some(otherwise) = otherwise {
                    self.check_stmt(otherwise, return_type)?;
                }
            }
            Stmt::LabeledStmt { label: _, stmt } => {
                self.check_stmt(stmt, return_type)?;
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.check_expr(condition)?;
                self.check_stmt(body, return_type)?;
            }
            Stmt::DoWhile {
                body, condition, ..
            } => {
                self.check_stmt(body, return_type)?;
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
                        return Err(SemErr::new(format!(
                            "Storage class not permitted on variable declaration in for-loop: '{}'",
                            name
                        )));
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

                self.check_stmt(body, return_type)?;
            }
            Stmt::Switch { control, body, .. } => {
                self.check_expr(control)?;
                self.check_stmt(body, return_type)?;
            }
            Stmt::Case {
                constant,
                body,
                label: _,
            } => {
                self.check_expr(constant)?;
                self.check_stmt(body, return_type)?;
            }
            Stmt::Default { body, .. } => {
                self.check_stmt(body, return_type)?;
            }
            Stmt::Null | Stmt::Goto { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => {}
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &mut Expr) -> SemanticResult<()> {
        match &mut expr.kind {
            ExprKind::Var(var) => {
                let var_type = &self.symbols.get(var).unwrap().t;

                match var_type {
                    Type::Func(_) => {
                        return Err(SemErr::new(format!(
                            "Tried to use function '{}' as a variable",
                            var
                        )));
                    }
                    _ => {
                        expr.set_type(var_type.clone());
                    }
                }
            }
            ExprKind::Unary {
                op,
                expr: ref mut body,
            } => {
                self.check_expr(body)?;
                match op {
                    UnaryOp::Not => expr.set_type(Type::Int),
                    _ => {
                        let body_type = body.get_type().unwrap();

                        if *op == UnaryOp::Complement && body_type.is_floating() {
                            return Err(SemErr::new(
                                "Can't take the bitwise complement of a double",
                            ));
                        }

                        expr.set_type(body_type)
                    }
                }
            }
            ExprKind::Binary { op, left, right } => {
                self.check_expr(left)?;
                self.check_expr(right)?;

                match op {
                    BinaryOp::BitshiftLeft | BinaryOp::BitshiftRight => {
                        // result has type of left operand
                        let left_type = left.get_type().unwrap();

                        expr.set_type(left_type);
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        // result always has value 0 or 1, int
                        expr.set_type(Type::Int);
                    }
                    _ => {
                        let left_type = left.get_type().unwrap();
                        let right_type = right.get_type().unwrap();
                        let common_type = get_common_type(left_type, right_type);

                        let left_cast = convert_to(left, common_type.clone());
                        let right_cast = convert_to(right, common_type.clone());

                        **left = left_cast;
                        **right = right_cast;

                        if *op == BinaryOp::Modulo && common_type.is_floating() {
                            return Err(SemErr::new("Invalid operands to modulo, double"));
                        }
                        
                        match op {
                            BinaryOp::Add
                            | BinaryOp::Subtract
                            | BinaryOp::Multiply
                            | BinaryOp::Divide
                            | BinaryOp::Modulo
                            | BinaryOp::BitwiseAnd
                            | BinaryOp::BitwiseOr
                            | BinaryOp::BitwiseXor => {
                                expr.set_type(common_type);
                            }
                            _ => {
                                expr.set_type(Type::Int);
                            }
                        }
                    }
                }
            }
            ExprKind::Assignment { lvalue, expr: body } => {
                self.check_expr(lvalue)?;
                self.check_expr(body)?;

                let left_type = lvalue.get_type().unwrap();

                let right_cast = convert_to(body, left_type.clone());

                **body = right_cast;
                expr.set_type(left_type);
            }
            ExprKind::CompoundAssignment {
                op,
                lvalue,
                expr: body,
            } => {
                self.check_expr(lvalue)?;
                self.check_expr(body)?;

                let left_type = lvalue.get_type().unwrap();
                let right_type = body.get_type().unwrap();

                let result_type = match op {
                    BinaryOp::BitshiftLeft | BinaryOp::BitshiftRight => left_type,
                    _ => {
                        let common_type = get_common_type(left_type, right_type);
                        let right_cast = convert_to(body, common_type.clone());
                        **body = right_cast;

                        common_type
                    }
                };

                expr.set_type(result_type);
            }
            ExprKind::Conditional {
                condition,
                then,
                otherwise,
            } => {
                self.check_expr(condition)?;
                self.check_expr(then)?;
                self.check_expr(otherwise)?;

                let common_type =
                    get_common_type(then.get_type().unwrap(), otherwise.get_type().unwrap());

                let then_cast = convert_to(then, common_type.clone());
                let otherwise_cast = convert_to(otherwise, common_type.clone());

                **then = then_cast;
                **otherwise = otherwise_cast;

                expr.set_type(common_type);
            }
            ExprKind::PostfixInc(body) => {
                self.check_expr(body)?;

                let body_type = body.get_type().unwrap();

                expr.set_type(body_type);
            }
            ExprKind::PostfixDec(body) => {
                self.check_expr(body)?;

                let body_type = body.get_type().unwrap();

                expr.set_type(body_type);
            }
            ExprKind::FunctionCall { func, args } => {
                let func_type = self.symbols.get(func).unwrap().t.clone();

                match func_type {
                    Type::Func(func_type) => {
                        if func_type.param_types.len() != args.len() {
                            return Err(SemErr::new(format!(
                                "Function '{}' called with wrong number of arguments, expected '{}' found '{}'",
                                 func, func_type.param_types.len(), args.len())));
                        }

                        let mut converted_args = vec![];

                        for (arg, param_type) in zip(&mut *args, func_type.param_types) {
                            self.check_expr(arg)?;

                            converted_args.push(convert_to(arg, param_type));
                        }

                        *args = converted_args;

                        expr.set_type(*func_type.return_type)
                    }
                    _ => {
                        return Err(SemErr::new(format!(
                            "Tried to use variable '{}' as function",
                            func
                        )))
                    }
                }
            }
            ExprKind::Constant(c) => match c {
                Constant::Int(_) => {
                    expr.set_type(Type::Int);
                }
                Constant::Long(_) => {
                    expr.set_type(Type::Long);
                }
                Constant::UInt(_) => {
                    expr.set_type(Type::UInt)
                }
                Constant::ULong(_) => {
                    expr.set_type(Type::ULong)
                }
                Constant::Float(_) => {
                    expr.set_type(Type::Float)
                }
                Constant::Double(_) => {
                    expr.set_type(Type::Double)
                }
            },
            ExprKind::Cast {
                target_type,
                expr: inner_expr,
            } => {
                self.check_expr(inner_expr)?;
                let target_type = target_type.clone();
                expr.set_type(target_type);
            }
        }

        Ok(())
    }
}

fn convert_to(expr: &Expr, target_type: Type) -> Expr {
    let mut cast = Expr::new(ExprKind::Cast {
        target_type: target_type.clone(),
        expr: Box::from(expr.clone()),
    });

    cast.set_type(target_type);

    cast
}

fn to_static_init(var_type: Type, init: Expr) -> SemanticResult<InitialVal> {
    match init.kind {
        ExprKind::Constant(val) => {
            let init_val = match const_convert(&var_type, val) {
                Constant::Int(val) => StaticInit::Int(val),
                Constant::Long(val) => StaticInit::Long(val),
                Constant::UInt(val) => StaticInit::UInt(val),
                Constant::ULong(val) => StaticInit::ULong(val),
                Constant::Float(val) => StaticInit::Float(val), 
                Constant::Double(val) => StaticInit::Double(val),
            };
            Ok(InitialVal::Initial(init_val))
        }
        _ => Err(SemErr::new(
            "Non-constant initializer on static variable".to_string(),
        )),
    }
}
