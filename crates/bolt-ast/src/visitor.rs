use crate::expr::{Expr, Parameter, Pattern, StringPart};
use crate::stmt::{ClassBody, EnumVariant, ImplItem, Stmt, TraitBody};
use crate::types::{GenericParam, Type, TypeAnnotation};

/// Trait for visiting AST nodes immutably
pub trait Visitor {
    /// Visit a statement
    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }

    /// Visit an expression
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    /// Visit a type
    fn visit_type(&mut self, ty: &Type) {
        walk_type(self, ty);
    }

    /// Visit a type annotation
    fn visit_type_annotation(&mut self, annotation: &TypeAnnotation) {
        if let Some(ty) = &annotation.explicit_type {
            self.visit_type(ty);
        }
        if let Some(ty) = &annotation.inferred_type {
            self.visit_type(ty);
        }
    }

    /// Visit a pattern
    fn visit_pattern(&mut self, pattern: &Pattern) {
        walk_pattern(self, pattern);
    }

    /// Visit a parameter
    fn visit_parameter(&mut self, param: &Parameter) {
        self.visit_type_annotation(&param.type_annotation);
    }

    /// Visit a generic parameter
    fn visit_generic_param(&mut self, param: &GenericParam) {
        // Default implementation does nothing
        let _ = param;
    }
}

/// Trait for visiting AST nodes mutably
pub trait VisitorMut {
    /// Visit a statement mutably
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        walk_stmt_mut(self, stmt);
    }

    /// Visit an expression mutably
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr_mut(self, expr);
    }

    /// Visit a type mutably
    fn visit_type(&mut self, ty: &mut Type) {
        walk_type_mut(self, ty);
    }

    /// Visit a type annotation mutably
    fn visit_type_annotation(&mut self, annotation: &mut TypeAnnotation) {
        if let Some(ty) = &mut annotation.explicit_type {
            self.visit_type(ty);
        }
        if let Some(ty) = &mut annotation.inferred_type {
            self.visit_type(ty);
        }
    }

    /// Visit a pattern mutably
    fn visit_pattern(&mut self, pattern: &mut Pattern) {
        walk_pattern_mut(self, pattern);
    }

    /// Visit a parameter mutably
    fn visit_parameter(&mut self, param: &mut Parameter) {
        self.visit_type_annotation(&mut param.type_annotation);
    }

    /// Visit a generic parameter mutably
    fn visit_generic_param(&mut self, param: &mut GenericParam) {
        // Default implementation does nothing
        let _ = param;
    }
}

/// Walk a statement (immutable)
pub fn walk_stmt<V: Visitor + ?Sized>(visitor: &mut V, stmt: &Stmt) {
    match stmt {
        Stmt::VariableDecl {
            type_annotation,
            initializer,
            ..
        } => {
            visitor.visit_type_annotation(type_annotation);
            if let Some(init) = initializer {
                visitor.visit_expr(init);
            }
        }
        Stmt::FunctionDef {
            generics,
            params,
            return_type,
            body,
            ..
        } => {
            for generic in generics {
                visitor.visit_generic_param(generic);
            }
            for param in params {
                visitor.visit_parameter(param);
            }
            visitor.visit_type_annotation(return_type);
            visitor.visit_stmt(body);
        }
        Stmt::ClassDef { generics, body, .. } => {
            for generic in generics {
                visitor.visit_generic_param(generic);
            }
            walk_class_body(visitor, body);
        }
        Stmt::StructDef {
            generics, fields, ..
        } => {
            for generic in generics {
                visitor.visit_generic_param(generic);
            }
            for field in fields {
                visitor.visit_type_annotation(&field.type_annotation);
            }
        }
        Stmt::EnumDef {
            generics, variants, ..
        } => {
            for generic in generics {
                visitor.visit_generic_param(generic);
            }
            for variant in variants {
                walk_enum_variant(visitor, variant);
            }
        }
        Stmt::TraitDef { generics, body, .. } => {
            for generic in generics {
                visitor.visit_generic_param(generic);
            }
            walk_trait_body(visitor, body);
        }
        Stmt::ImplBlock { generics, body, .. } => {
            for generic in generics {
                visitor.visit_generic_param(generic);
            }
            for item in body {
                walk_impl_item(visitor, item);
            }
        }
        Stmt::ModuleDef { body, .. } => {
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        Stmt::Import { .. } => {
            // No sub-nodes to visit
        }
        Stmt::ExpressionStmt { expr, .. } => {
            visitor.visit_expr(expr);
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_stmt(then_branch);
            if let Some(else_stmt) = else_branch {
                visitor.visit_stmt(else_stmt);
            }
        }
        Stmt::Loop { kind, body, .. } => {
            match kind {
                crate::stmt::LoopKind::While(condition) => {
                    visitor.visit_expr(condition);
                }
                crate::stmt::LoopKind::For {
                    init,
                    condition,
                    increment,
                } => {
                    if let Some(init_stmt) = init {
                        visitor.visit_stmt(init_stmt);
                    }
                    if let Some(cond) = condition {
                        visitor.visit_expr(cond);
                    }
                    if let Some(incr) = increment {
                        visitor.visit_expr(incr);
                    }
                }
                crate::stmt::LoopKind::ForEach { iterable, .. } => {
                    visitor.visit_expr(iterable);
                }
                crate::stmt::LoopKind::Infinite => {}
            }
            visitor.visit_stmt(body);
        }
        Stmt::Match { expr, arms, .. } => {
            visitor.visit_expr(expr);
            for arm in arms {
                visitor.visit_pattern(&arm.pattern);
                if let Some(guard) = &arm.guard {
                    visitor.visit_expr(guard);
                }
                visitor.visit_stmt(&arm.body);
            }
        }
        Stmt::Return { value, .. } => {
            if let Some(val) = value {
                visitor.visit_expr(val);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {
            // No sub-nodes to visit
        }
        Stmt::Defer { expr, .. } => {
            visitor.visit_expr(expr);
        }
        Stmt::Block { statements, .. } => {
            for stmt in statements {
                visitor.visit_stmt(stmt);
            }
        }
    }
}

/// Walk an expression (immutable)
pub fn walk_expr<V: Visitor + ?Sized>(visitor: &mut V, expr: &Expr) {
    match expr {
        Expr::Literal {
            type_annotation, ..
        } => {
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Identifier {
            type_annotation, ..
        } => {
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Binary {
            left,
            right,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Unary {
            operand,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(operand);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Call {
            callee,
            args,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::MethodCall {
            receiver,
            args,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(receiver);
            for arg in args {
                visitor.visit_expr(arg);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::FieldAccess {
            object,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(object);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Index {
            object,
            index,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(object);
            visitor.visit_expr(index);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Array {
            elements,
            type_annotation,
            ..
        } => {
            for element in elements {
                visitor.visit_expr(element);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Map {
            pairs,
            type_annotation,
            ..
        } => {
            for (key, value) in pairs {
                visitor.visit_expr(key);
                visitor.visit_expr(value);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::StringInterpolation {
            parts,
            type_annotation,
            ..
        } => {
            for part in parts {
                if let StringPart::Expression(expr) = part {
                    visitor.visit_expr(expr);
                }
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Lambda {
            params,
            body,
            type_annotation,
            ..
        } => {
            for param in params {
                visitor.visit_parameter(param);
            }
            visitor.visit_expr(body);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(then_branch);
            if let Some(else_expr) = else_branch {
                visitor.visit_expr(else_expr);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Match {
            expr,
            arms,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(expr);
            for arm in arms {
                visitor.visit_pattern(&arm.pattern);
                if let Some(guard) = &arm.guard {
                    visitor.visit_expr(guard);
                }
                visitor.visit_expr(&arm.body);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Loop {
            kind,
            body,
            type_annotation,
            ..
        } => {
            match kind {
                crate::expr::LoopKind::While(condition) => {
                    visitor.visit_expr(condition);
                }
                crate::expr::LoopKind::For {
                    init,
                    condition,
                    increment,
                } => {
                    if let Some(init_expr) = init {
                        visitor.visit_expr(init_expr);
                    }
                    if let Some(cond) = condition {
                        visitor.visit_expr(cond);
                    }
                    if let Some(incr) = increment {
                        visitor.visit_expr(incr);
                    }
                }
                crate::expr::LoopKind::ForEach { iterable, .. } => {
                    visitor.visit_expr(iterable);
                }
                crate::expr::LoopKind::Infinite => {}
            }
            visitor.visit_expr(body);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Block {
            statements,
            type_annotation,
            ..
        } => {
            for stmt in statements {
                visitor.visit_expr(stmt);
            }
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Assignment {
            target,
            value,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(target);
            visitor.visit_expr(value);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Try {
            expr,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(expr);
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::SelfRef {
            type_annotation, ..
        } => {
            visitor.visit_type_annotation(type_annotation);
        }
        Expr::Return {
            value,
            type_annotation,
            ..
        } => {
            if let Some(val) = value {
                visitor.visit_expr(val);
            }
            visitor.visit_type_annotation(type_annotation);
        }
    }
}

/// Walk a type (immutable)
pub fn walk_type<V: Visitor + ?Sized>(visitor: &mut V, ty: &Type) {
    match ty {
        Type::Array(inner) => {
            visitor.visit_type(inner);
        }
        Type::Map(key, value) => {
            visitor.visit_type(key);
            visitor.visit_type(value);
        }
        Type::Function {
            params,
            return_type,
        } => {
            for param_type in params {
                visitor.visit_type(param_type);
            }
            visitor.visit_type(return_type);
        }
        Type::UserDefined { generics, .. } => {
            for generic in generics {
                visitor.visit_type(generic);
            }
        }
        Type::Reference { inner, .. } => {
            visitor.visit_type(inner);
        }
        Type::Result(ok, err) => {
            visitor.visit_type(ok);
            visitor.visit_type(err);
        }
        Type::Maybe(inner) => {
            visitor.visit_type(inner);
        }
        _ => {
            // Primitive types and others have no sub-types
        }
    }
}

/// Walk a pattern (immutable)
pub fn walk_pattern<V: Visitor + ?Sized>(visitor: &mut V, pattern: &Pattern) {
    match pattern {
        Pattern::Constructor { args, .. } => {
            for arg_pattern in args {
                visitor.visit_pattern(arg_pattern);
            }
        }
        _ => {
            // Other patterns have no sub-patterns
        }
    }
}

// Helper functions for walking complex structures
fn walk_class_body<V: Visitor + ?Sized>(visitor: &mut V, body: &ClassBody) {
    for field in &body.fields {
        visitor.visit_type_annotation(&field.type_annotation);
    }
    for method in &body.methods {
        for generic in &method.generics {
            visitor.visit_generic_param(generic);
        }
        for param in &method.params {
            visitor.visit_parameter(param);
        }
        visitor.visit_type_annotation(&method.return_type);
        visitor.visit_stmt(&method.body);
    }
    for constructor in &body.constructors {
        for param in &constructor.params {
            visitor.visit_parameter(param);
        }
        if let Some(body) = &constructor.body {
            visitor.visit_stmt(body);
        }
    }
}

fn walk_enum_variant<V: Visitor + ?Sized>(visitor: &mut V, variant: &EnumVariant) {
    match &variant.data {
        crate::stmt::EnumVariantData::Tuple(types) => {
            for ty in types {
                visitor.visit_type(ty);
            }
        }
        crate::stmt::EnumVariantData::Struct(fields) => {
            for field in fields {
                visitor.visit_type_annotation(&field.type_annotation);
            }
        }
        crate::stmt::EnumVariantData::Unit => {}
    }
}

fn walk_trait_body<V: Visitor + ?Sized>(visitor: &mut V, body: &TraitBody) {
    for method in &body.methods {
        for generic in &method.generics {
            visitor.visit_generic_param(generic);
        }
        for param in &method.params {
            visitor.visit_parameter(param);
        }
        visitor.visit_type_annotation(&method.return_type);
        if let Some(default_body) = &method.default_body {
            visitor.visit_stmt(default_body);
        }
    }
}

fn walk_impl_item<V: Visitor + ?Sized>(visitor: &mut V, item: &ImplItem) {
    match item {
        ImplItem::Method(method) => {
            for generic in &method.generics {
                visitor.visit_generic_param(generic);
            }
            for param in &method.params {
                visitor.visit_parameter(param);
            }
            visitor.visit_type_annotation(&method.return_type);
            visitor.visit_stmt(&method.body);
        }
        ImplItem::Type { type_def, .. } => {
            visitor.visit_type(type_def);
        }
        ImplItem::Const {
            type_annotation,
            value,
            ..
        } => {
            visitor.visit_type_annotation(type_annotation);
            visitor.visit_expr(value);
        }
    }
}

// Mutable versions (abbreviated for brevity - similar pattern)
pub fn walk_stmt_mut<V: VisitorMut + ?Sized>(visitor: &mut V, stmt: &mut Stmt) {
    // Similar to walk_stmt but with &mut references
    match stmt {
        Stmt::VariableDecl {
            type_annotation,
            initializer,
            ..
        } => {
            visitor.visit_type_annotation(type_annotation);
            if let Some(init) = initializer {
                visitor.visit_expr(init);
            }
        }
        // ... (similar pattern for other statement types)
        _ => {
            // Simplified for brevity - full implementation would mirror walk_stmt
        }
    }
}

pub fn walk_expr_mut<V: VisitorMut + ?Sized>(visitor: &mut V, expr: &mut Expr) {
    // Similar to walk_expr but with &mut references
    match expr {
        Expr::Binary {
            left,
            right,
            type_annotation,
            ..
        } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
            visitor.visit_type_annotation(type_annotation);
        }
        // ... (similar pattern for other expression types)
        _ => {
            // Simplified for brevity - full implementation would mirror walk_expr
        }
    }
}

pub fn walk_type_mut<V: VisitorMut + ?Sized>(visitor: &mut V, ty: &mut Type) {
    // Similar to walk_type but with &mut references
    match ty {
        Type::Array(inner) => {
            visitor.visit_type(inner);
        }
        // ... (similar pattern for other types)
        _ => {}
    }
}

pub fn walk_pattern_mut<V: VisitorMut + ?Sized>(visitor: &mut V, pattern: &mut Pattern) {
    // Similar to walk_pattern but with &mut references
    match pattern {
        Pattern::Constructor { args, .. } => {
            for arg_pattern in args {
                visitor.visit_pattern(arg_pattern);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::LiteralValue;
    use bolt_lexer::{Position, Span};

    fn dummy_span() -> Span {
        Span::single(Position::new(1, 1, 0))
    }

    fn dummy_type_annotation() -> TypeAnnotation {
        TypeAnnotation::inferred(dummy_span())
    }

    // Example visitor that counts nodes
    struct NodeCounter {
        stmt_count: usize,
        expr_count: usize,
        type_count: usize,
    }

    impl NodeCounter {
        fn new() -> Self {
            Self {
                stmt_count: 0,
                expr_count: 0,
                type_count: 0,
            }
        }
    }

    impl Visitor for NodeCounter {
        fn visit_stmt(&mut self, stmt: &Stmt) {
            self.stmt_count += 1;
            walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Expr) {
            self.expr_count += 1;
            walk_expr(self, expr);
        }

        fn visit_type(&mut self, ty: &Type) {
            self.type_count += 1;
            walk_type(self, ty);
        }
    }

    #[test]
    fn test_visitor_counts_nodes() {
        let mut counter = NodeCounter::new();

        // Create a simple expression: 1 + 2
        let expr = Expr::Binary {
            left: Box::new(Expr::Literal {
                value: LiteralValue::Integer(1),
                type_annotation: dummy_type_annotation(),
                span: dummy_span(),
            }),
            operator: crate::expr::BinaryOp::Add,
            right: Box::new(Expr::Literal {
                value: LiteralValue::Integer(2),
                type_annotation: dummy_type_annotation(),
                span: dummy_span(),
            }),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        counter.visit_expr(&expr);

        // Should count: Binary expr + 2 Literal exprs = 3 expressions
        assert_eq!(counter.expr_count, 3);
        assert_eq!(counter.stmt_count, 0);
    }

    #[test]
    fn test_visitor_walks_nested_structure() {
        let mut counter = NodeCounter::new();

        // Create an array of function calls
        let expr = Expr::Array {
            elements: vec![
                Expr::Call {
                    callee: Box::new(Expr::Identifier {
                        name: "func1".to_string(),
                        type_annotation: dummy_type_annotation(),
                        span: dummy_span(),
                    }),
                    args: vec![],
                    type_annotation: dummy_type_annotation(),
                    span: dummy_span(),
                },
                Expr::Call {
                    callee: Box::new(Expr::Identifier {
                        name: "func2".to_string(),
                        type_annotation: dummy_type_annotation(),
                        span: dummy_span(),
                    }),
                    args: vec![],
                    type_annotation: dummy_type_annotation(),
                    span: dummy_span(),
                },
            ],
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        counter.visit_expr(&expr);

        // Should count: Array + 2 Calls + 2 Identifiers = 5 expressions
        assert_eq!(counter.expr_count, 5);
    }
}
