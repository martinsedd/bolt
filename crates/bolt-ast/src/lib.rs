//! Bolt Programming Language Abstract Syntax Tree
//!
//! This crate provides AST node definitions for the Bolt Programming Language,
//! including expressions, statements, declarations, and type representations.

mod expr;
mod stmt;
mod types;
mod visitor;
// Placeholder modules for future implementation
// mod decl;
// mod display;

// Re-export public API
pub use expr::{
    BinaryOp, Expr, LiteralValue, LoopKind as ExprLoopKind, MatchArm as ExprMatchArm, Parameter,
    Pattern, StringPart, UnaryOp,
};
pub use stmt::{
    AccessorDef, ClassBody, ClassField, ClassMethod, Constructor, EnumVariant, EnumVariantData,
    ImplItem, ImportPath, LoopKind, MatchArm, Stmt, StructField, TraitBody, TraitMethod,
    Visibility,
};
pub use types::{GenericParam, TraitBound, Type, TypeAnnotation};
pub use visitor::{Visitor, VisitorMut, walk_expr, walk_pattern, walk_stmt, walk_type};

// Future re-exports:
// pub use decl::*;
// pub use display::*;

// Future re-
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_public_api() {
        // Test that all public items are accessible
        let int_type = Type::Int;
        assert!(int_type.is_primitive());

        let array_type = Type::Array(Box::new(Type::String));
        assert!(array_type.is_collection());

        // Test expression creation
        let literal = LiteralValue::Integer(69);
        assert_eq!(literal.get_type(), Type::Int);

        // Test statement creation
        let visibility = Visibility::Public;
        assert!(visibility.is_public());

        // Test visitor traits are available;
        struct DummyVisitor;
        impl Visitor for DummyVisitor {}
        let mut _visitor = DummyVisitor;
    }
}
