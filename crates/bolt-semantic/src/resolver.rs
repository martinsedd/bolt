use crate::{ScopeKind, ScopeManager, SemanticError, SemanticErrors, Symbol, SymbolKind, TypeKind};
use bolt_ast::{Expr, Stmt, Type, Visibility};

/// Program representation for semantic analysis
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Box<Stmt>>,
}

/// Semantic analyzer that walks the AST and performs symbol resolution
#[derive(Debug)]
pub struct SemanticAnalyzer {
    /// Scope manager for symbol resolution
    scope_manager: ScopeManager,

    /// Collected errors and warnings
    pub errors: SemanticErrors,

    /// Current function return type (for return statement checking)
    current_function_return_type: Option<Type>,

    /// Whether we're currently in an async context
    in_async_context: bool,

    /// Whether we're currently in a pure function
    in_pure_context: bool,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            scope_manager: ScopeManager::new(),
            errors: SemanticErrors::new(),
            current_function_return_type: None,
            in_async_context: false,
            in_pure_context: false,
        }
    }

    /// Analyze a complete program
    pub fn analyze(&mut self, program: &Program) -> Result<(), SemanticErrors> {
        // First pass: collect all top-level declarations
        self.collect_declarations(&program.statements);

        // Second pass: analyze all statements
        for stmt in &program.statements {
            self.analyze_statement(stmt);
        }

        // Check for unused symbols
        self.check_unused_symbols();

        if self.errors.has_errors() {
            Err(std::mem::take(&mut self.errors))
        } else {
            Ok(())
        }
    }

    /// Get the scope manager (for testing/debugging)
    pub fn scope_manager(&self) -> &ScopeManager {
        &self.scope_manager
    }

    /// Get collected errors
    pub fn errors(&self) -> &SemanticErrors {
        &self.errors
    }

    /// First pass: collect all top-level declarations to handle forward references
    fn collect_declarations(&mut self, statements: &[Box<Stmt>]) {
        for stmt in statements {
            match stmt.as_ref() {
                Stmt::FunctionDef {
                    name,
                    params,
                    return_type,
                    visibility,
                    span,
                    is_pure,
                    is_async,
                    ..
                } => {
                    let param_types = params
                        .iter()
                        .map(|p| {
                            p.type_annotation
                                .current_type()
                                .cloned()
                                .unwrap_or(Type::Unknown)
                        })
                        .collect();

                    let return_ty = return_type.current_type().cloned().unwrap_or(Type::Unit);

                    let symbol = Symbol::new(
                        name.clone(),
                        SymbolKind::Function {
                            params: param_types,
                            return_type: return_ty,
                            is_pure: *is_pure,
                            is_async: *is_async,
                        },
                        visibility.clone(),
                        *span,
                        self.scope_manager.current_scope(),
                    );

                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: name.clone(),
                            span: *span,
                            previous_span: *span, // TODO: track actual previous span
                        });
                    }
                }

                Stmt::ClassDef {
                    name,
                    visibility,
                    span,
                    ..
                } => {
                    let symbol = Symbol::new(
                        name.clone(),
                        SymbolKind::Type {
                            type_def: Type::UserDefined {
                                name: name.clone(),
                                generics: vec![],
                            },
                            kind: TypeKind::Class,
                        },
                        visibility.clone(),
                        *span,
                        self.scope_manager.current_scope(),
                    );

                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: name.clone(),
                            span: *span,
                            previous_span: *span,
                        });
                    }
                }

                Stmt::StructDef {
                    name,
                    visibility,
                    span,
                    ..
                } => {
                    let symbol = Symbol::new(
                        name.clone(),
                        SymbolKind::Type {
                            type_def: Type::UserDefined {
                                name: name.clone(),
                                generics: vec![],
                            },
                            kind: TypeKind::Struct,
                        },
                        visibility.clone(),
                        *span,
                        self.scope_manager.current_scope(),
                    );

                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: name.clone(),
                            span: *span,
                            previous_span: *span,
                        });
                    }
                }

                Stmt::EnumDef {
                    name,
                    visibility,
                    span,
                    ..
                } => {
                    let symbol = Symbol::new(
                        name.clone(),
                        SymbolKind::Type {
                            type_def: Type::UserDefined {
                                name: name.clone(),
                                generics: vec![],
                            },
                            kind: TypeKind::Enum,
                        },
                        visibility.clone(),
                        *span,
                        self.scope_manager.current_scope(),
                    );

                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: name.clone(),
                            span: *span,
                            previous_span: *span,
                        });
                    }
                }

                Stmt::TraitDef {
                    name,
                    visibility,
                    span,
                    ..
                } => {
                    let symbol = Symbol::new(
                        name.clone(),
                        SymbolKind::Type {
                            type_def: Type::UserDefined {
                                name: name.clone(),
                                generics: vec![],
                            },
                            kind: TypeKind::Trait,
                        },
                        visibility.clone(),
                        *span,
                        self.scope_manager.current_scope(),
                    );

                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: name.clone(),
                            span: *span,
                            previous_span: *span,
                        });
                    }
                }

                _ => {} // Other statements handled in second pass
            }
        }
    }

    /// Analyze a statement
    fn analyze_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VariableDecl {
                name,
                mutable,
                type_annotation,
                initializer,
                span,
                ..
            } => {
                // Analyze initializer first
                if let Some(init) = initializer {
                    self.analyze_expression(init);
                }

                // Add variable to current scope
                let var_type = type_annotation
                    .current_type()
                    .cloned()
                    .unwrap_or(Type::Unknown);
                let symbol = Symbol::new(
                    name.clone(),
                    SymbolKind::Variable {
                        mutable: *mutable,
                        var_type,
                    },
                    Visibility::Private, // Local variables are always private
                    *span,
                    self.scope_manager.current_scope(),
                );

                if let Err(_) = self.scope_manager.add_symbol(symbol) {
                    self.errors.add(SemanticError::DuplicateSymbol {
                        name: name.clone(),
                        span: *span,
                        previous_span: *span,
                    });
                }
            }

            Stmt::ExpressionStmt { expr, .. } => {
                self.analyze_expression(expr);
            }

            Stmt::FunctionDef {
                name,
                params,
                body,
                return_type,
                span,
                is_async,
                is_pure,
                ..
            } => {
                // First, add the function to the current scope (for nested functions)
                let param_types = params
                    .iter()
                    .map(|p| {
                        p.type_annotation
                            .current_type()
                            .cloned()
                            .unwrap_or(Type::Unknown)
                    })
                    .collect();

                let return_ty = return_type.current_type().cloned().unwrap_or(Type::Unit);

                let symbol = Symbol::new(
                    name.clone(),
                    SymbolKind::Function {
                        params: param_types,
                        return_type: return_ty,
                        is_pure: *is_pure,
                        is_async: *is_async,
                    },
                    Visibility::Private, // Nested functions are private by default
                    *span,
                    self.scope_manager.current_scope(),
                );

                // Only add if not already collected (to avoid duplicates with top-level functions)
                if self.scope_manager.resolve_symbol(name).is_none() {
                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: name.clone(),
                            span: *span,
                            previous_span: *span,
                        });
                    }
                }

                // Create function scope for the body
                let fn_scope = self.scope_manager.create_scope(
                    ScopeKind::Function {
                        name: name.clone(),
                        is_async: *is_async,
                    },
                    *span,
                );

                self.scope_manager.enter_scope(fn_scope);

                // Add parameters to function scope
                for param in params {
                    let param_type = param
                        .type_annotation
                        .current_type()
                        .cloned()
                        .unwrap_or(Type::Unknown);
                    let symbol = Symbol::new(
                        param.name.clone(),
                        SymbolKind::Variable {
                            mutable: false, // Parameters are immutable by default
                            var_type: param_type,
                        },
                        Visibility::Private,
                        param.span,
                        fn_scope,
                    );

                    if let Err(_) = self.scope_manager.add_symbol(symbol) {
                        self.errors.add(SemanticError::DuplicateSymbol {
                            name: param.name.clone(),
                            span: param.span,
                            previous_span: param.span,
                        });
                    }
                }

                // Set return type context
                let prev_return_type = self.current_function_return_type.clone();
                self.current_function_return_type = return_type.current_type().cloned();

                // Analyze function body
                self.analyze_statement(body);

                // Restore context
                self.current_function_return_type = prev_return_type;
                self.scope_manager.exit_scope();
            }

            // ... rest of the match arms remain the same
            Stmt::ClassDef {
                name, body, span, ..
            } => {
                let class_scope = self
                    .scope_manager
                    .create_scope(ScopeKind::Class { name: name.clone() }, *span);

                self.scope_manager.enter_scope(class_scope);

                // Analyze class body
                self.analyze_class_body(body);

                self.scope_manager.exit_scope();
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.analyze_expression(condition);
                self.analyze_statement(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.analyze_statement(else_stmt);
                }
            }

            Stmt::Block { statements, span } => {
                let block_scope = self.scope_manager.create_scope(ScopeKind::Block, *span);
                self.scope_manager.enter_scope(block_scope);

                for stmt in statements {
                    self.analyze_statement(stmt);
                }

                self.scope_manager.exit_scope();
            }

            Stmt::Return { value, .. } => {
                if let Some(expr) = value {
                    self.analyze_expression(expr);
                }

                // TODO: Type checking for return values
            }

            Stmt::ImplBlock {
                type_name, body, ..
            } => {
                let impl_scope = self.scope_manager.create_scope(
                    ScopeKind::Impl {
                        type_name: type_name.clone(),
                        trait_name: None, // TODO: handle trait implementations
                    },
                    stmt.span(),
                );

                self.scope_manager.enter_scope(impl_scope);

                for item in body {
                    self.analyze_impl_item(item);
                }

                self.scope_manager.exit_scope();
            }

            // Simplified handling for other statements
            _ => {
                // TODO: Implement remaining statement types
            }
        }
    }
    /// Analyze an expression
    fn analyze_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Identifier { name, span, .. } => {
                if let Some(_symbol) = self.scope_manager.resolve_symbol(name) {
                    // Mark symbol as used
                    self.scope_manager.mark_symbol_used(name);
                } else {
                    self.errors.add(SemanticError::UndefinedSymbol {
                        name: name.clone(),
                        span: *span,
                    });
                }
            }

            Expr::Call {
                callee, args, span, ..
            } => {
                self.analyze_expression(callee);

                for arg in args {
                    self.analyze_expression(arg);
                }

                // Check function call validity
                if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if let Some(symbol) = self.scope_manager.resolve_symbol(name) {
                        if let SymbolKind::Function { params, .. } = &symbol.kind {
                            if args.len() != params.len() {
                                self.errors.add(SemanticError::ArgumentCountMismatch {
                                    name: name.clone(),
                                    arg_count: args.len(),
                                    expected_count: params.len(),
                                    span: *span,
                                });
                            }
                        }
                    }
                }
            }

            Expr::MethodCall { receiver, args, .. } => {
                self.analyze_expression(receiver);
                for arg in args {
                    self.analyze_expression(arg);
                }
                // TODO: Method resolution
            }

            Expr::FieldAccess { object, .. } => {
                self.analyze_expression(object);
                // TODO: Field access validation
            }

            Expr::Binary { left, right, .. } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
            }

            Expr::Unary { operand, .. } => {
                self.analyze_expression(operand);
            }

            Expr::Array { elements, .. } => {
                for element in elements {
                    self.analyze_expression(element);
                }
            }

            Expr::Index { object, index, .. } => {
                self.analyze_expression(object);
                self.analyze_expression(index);
            }

            Expr::Assignment { target, value, .. } => {
                self.analyze_expression(target);
                self.analyze_expression(value);

                // Check if target is assignable
                if let Expr::Identifier { name, span, .. } = target.as_ref() {
                    if let Some(symbol) = self.scope_manager.resolve_symbol(name) {
                        if let SymbolKind::Variable { mutable, .. } = &symbol.kind {
                            if !mutable {
                                self.errors.add(SemanticError::AssignToImmutable {
                                    name: name.clone(),
                                    span: *span,
                                });
                            }
                        }
                    }
                }
            }

            // Handle other expression types
            _ => {
                // TODO: Implement remaining expression types
            }
        }
    }

    /// Analyze class body
    fn analyze_class_body(&mut self, _body: &bolt_ast::ClassBody) {
        // TODO: Implement class body analysis
        // - Fields
        // - Methods
        // - Constructors
        // - Inheritance checking
    }

    /// Analyze implementation item
    fn analyze_impl_item(&mut self, item: &bolt_ast::ImplItem) {
        match item {
            bolt_ast::ImplItem::Method(method) => {
                // Analyze method similar to function
                let method_scope = self.scope_manager.create_scope(
                    ScopeKind::Function {
                        name: method.name.clone(),
                        is_async: method.is_async,
                    },
                    method.span,
                );

                self.scope_manager.enter_scope(method_scope);

                // Add parameters
                for param in &method.params {
                    let param_type = param
                        .type_annotation
                        .current_type()
                        .cloned()
                        .unwrap_or(Type::Unknown);
                    let symbol = Symbol::new(
                        param.name.clone(),
                        SymbolKind::Variable {
                            mutable: false,
                            var_type: param_type,
                        },
                        Visibility::Private,
                        param.span,
                        method_scope,
                    );

                    let _ = self.scope_manager.add_symbol(symbol);
                }

                // Analyze method body
                self.analyze_statement(&method.body);

                self.scope_manager.exit_scope();
            }
            _ => {
                // TODO: Handle other impl items
            }
        }
    }

    /// Check for unused symbols and generate warnings
    fn check_unused_symbols(&mut self) {
        let unused_symbols = self.scope_manager.get_unused_symbols();

        for symbol in unused_symbols {
            match &symbol.kind {
                SymbolKind::Variable { .. } => {
                    // Skip function parameters and special variables
                    if !symbol.name.starts_with('_') {
                        self.errors.add(SemanticError::UnusedVariable {
                            name: symbol.name.clone(),
                            span: symbol.definition_span,
                        });
                    }
                }
                SymbolKind::Function { .. } => {
                    // Skip main function and exported functions
                    if symbol.name != "main" && symbol.visibility != Visibility::Public {
                        self.errors.add(SemanticError::UnusedFunction {
                            name: symbol.name.clone(),
                            span: symbol.definition_span,
                        });
                    }
                }
                _ => {} // Don't warn about unused types/modules for now
            }
        }
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bolt_ast::*;
    use bolt_lexer::{Position, Span};

    fn create_test_span() -> Span {
        Span::new(Position::default(), Position::default())
    }

    #[test]
    fn test_undefined_variable() {
        let mut analyzer = SemanticAnalyzer::new();

        // Create expression using undefined variable
        let expr = Expr::Identifier {
            name: "undefined_var".to_string(),
            type_annotation: TypeAnnotation::inferred(create_test_span()),
            span: create_test_span(),
        };

        analyzer.analyze_expression(&expr);

        assert!(analyzer.errors.has_errors());
        assert_eq!(analyzer.errors.error_count(), 1);
    }

    #[test]
    fn test_function_declaration() {
        let mut analyzer = SemanticAnalyzer::new();

        let program = Program {
            statements: vec![Box::new(Stmt::FunctionDef {
                name: "test_fn".to_string(),
                is_pure: false,
                is_async: false,
                generics: vec![],
                params: vec![],
                return_type: TypeAnnotation::explicit(Type::Int, create_test_span()),
                body: Box::new(Stmt::Block {
                    statements: vec![],
                    span: create_test_span(),
                }),
                visibility: Visibility::Public,
                span: create_test_span(),
            })],
        };

        let result = analyzer.analyze(&program);
        assert!(result.is_ok());

        // Function should be in symbol table
        assert!(analyzer.scope_manager.resolve_symbol("test_fn").is_some());
    }

    #[test]
    fn test_duplicate_declaration() {
        let mut analyzer = SemanticAnalyzer::new();

        let program = Program {
            statements: vec![
                Box::new(Stmt::FunctionDef {
                    name: "duplicate".to_string(),
                    is_pure: false,
                    is_async: false,
                    generics: vec![],
                    params: vec![],
                    return_type: TypeAnnotation::explicit(Type::Unit, create_test_span()),
                    body: Box::new(Stmt::Block {
                        statements: vec![],
                        span: create_test_span(),
                    }),
                    visibility: Visibility::Public,
                    span: create_test_span(),
                }),
                Box::new(Stmt::FunctionDef {
                    name: "duplicate".to_string(),
                    is_pure: false,
                    is_async: false,
                    generics: vec![],
                    params: vec![],
                    return_type: TypeAnnotation::explicit(Type::Unit, create_test_span()),
                    body: Box::new(Stmt::Block {
                        statements: vec![],
                        span: create_test_span(),
                    }),
                    visibility: Visibility::Public,
                    span: create_test_span(),
                }),
            ],
        };

        let result = analyzer.analyze(&program);
        assert!(result.is_err());

        if let Err(errors) = result {
            assert!(errors.has_errors());
            assert_eq!(errors.error_count(), 1);
        }
    }
}
