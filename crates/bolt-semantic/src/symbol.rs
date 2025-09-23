use bolt_ast::{Type, Visibility};
use bolt_lexer::Span;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SymbolKind {
    /// Variable or parameter
    Variable { mutable: bool, var_type: Type },

    /// Function definition
    Function {
        params: Vec<Type>,
        return_type: Type,
        is_pure: bool,
        is_async: bool,
    },

    /// Type definition (class, struct, enum, trait)
    Type { type_def: Type, kind: TypeKind },

    /// Module
    Module { path: String },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeKind {
    Class,
    Struct,
    Enum,
    Trait,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub visibility: Visibility,
    pub definition_span: Span,
    pub scope_id: ScopeId,
    pub is_used: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ScopeId(pub u32);

impl Symbol {
    pub fn new(
        name: String,
        kind: SymbolKind,
        visibility: Visibility,
        definition_span: Span,
        scope_id: ScopeId,
    ) -> Self {
        Self {
            name,
            kind,
            visibility,
            definition_span,
            scope_id,
            is_used: false,
        }
    }

    pub fn mark_used(&mut self) {
        self.is_used = true;
    }

    /// Check if this symbol is accessible from the given scope
    pub fn is_accessible_from(&self, _from_scope: ScopeId) -> bool {
        // TODO: Implement proper visibility checking
        true
    }
}
