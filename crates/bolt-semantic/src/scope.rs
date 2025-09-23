use crate::{ScopeId, Symbol};
use bolt_lexer::Span;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Different types of scopes in Bolt
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ScopeKind {
    /// Global/module scope
    Global,

    /// Function scope
    Function { name: String, is_async: bool },

    /// Class scope
    Class { name: String },

    /// Trait scope
    Trait { name: String },

    /// Block scope (if, loop, etc.)
    Block,

    /// Impl block scope
    Impl {
        type_name: String,
        trait_name: Option<String>,
    },
}

/// A scope containing symbols and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scope {
    /// Unique identifier for this scope
    pub id: ScopeId,

    /// What kind of scope this is
    pub kind: ScopeKind,

    /// Parent scope (None for global scope)
    pub parent: Option<ScopeId>,

    /// Symbols defined directly in this scope
    /// Using IndexMap to preserve declaration order
    pub symbols: IndexMap<String, Symbol>,

    /// Child scopes
    pub children: Vec<ScopeId>,

    /// Span where this scope is defined
    pub span: Span,
}

impl Scope {
    pub fn new(id: ScopeId, kind: ScopeKind, parent: Option<ScopeId>, span: Span) -> Self {
        Self {
            id,
            kind,
            parent,
            symbols: IndexMap::new(),
            children: Vec::new(),
            span,
        }
    }

    /// Add a symbol to this scope
    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(&symbol.name) {
            return Err(format!(
                "Symbol '{}' already defined in this scope",
                symbol.name
            ));
        }

        self.symbols.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    /// Get a symbol from this scope (not including parent scopes)
    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    /// Get a mutable reference to a symbol in this scope
    pub fn get_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symbols.get_mut(name)
    }

    /// Check if a symbol exists in this scope
    pub fn has_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Add a child scope
    pub fn add_child(&mut self, child_id: ScopeId) {
        self.children.push(child_id);
    }
}

/// Manages all scopes and provides symbol resolution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScopeManager {
    /// All scopes indexed by their ID
    scopes: HashMap<ScopeId, Scope>,

    /// Current scope being processed
    current_scope: ScopeId,

    /// Next available scope ID
    next_scope_id: u32,

    /// Global scope ID (always ScopeId(0))
    global_scope: ScopeId,
}

impl ScopeManager {
    pub fn new() -> Self {
        let global_scope = ScopeId(0);
        let mut scopes = HashMap::new();

        // Create global scope
        let global = Scope::new(
            global_scope,
            ScopeKind::Global,
            None,
            Span::default(), // Global scope has no specific location
        );

        scopes.insert(global_scope, global);

        Self {
            scopes,
            current_scope: global_scope,
            next_scope_id: 1,
            global_scope,
        }
    }

    /// Create a new scope and return its ID
    pub fn create_scope(&mut self, kind: ScopeKind, span: Span) -> ScopeId {
        let scope_id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;

        let scope = Scope::new(scope_id, kind, Some(self.current_scope), span);

        // Add as child to current scope
        if let Some(current) = self.scopes.get_mut(&self.current_scope) {
            current.add_child(scope_id);
        }

        self.scopes.insert(scope_id, scope);
        scope_id
    }

    /// Enter a scope (make it current)
    pub fn enter_scope(&mut self, scope_id: ScopeId) {
        self.current_scope = scope_id;
    }

    /// Exit current scope (return to parent)
    pub fn exit_scope(&mut self) {
        if let Some(scope) = self.scopes.get(&self.current_scope) {
            if let Some(parent) = scope.parent {
                self.current_scope = parent;
            }
        }
    }

    /// Get current scope ID
    pub fn current_scope(&self) -> ScopeId {
        self.current_scope
    }

    /// Get global scope ID
    pub fn global_scope(&self) -> ScopeId {
        self.global_scope
    }

    /// Get a scope by ID
    pub fn get_scope(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&id)
    }

    /// Get a mutable scope by ID
    pub fn get_scope_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(&id)
    }

    /// Add a symbol to the current scope
    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        let current_scope_id = self.current_scope;
        if let Some(scope) = self.scopes.get_mut(&current_scope_id) {
            scope.add_symbol(symbol)
        } else {
            Err("Current scope not found".to_string())
        }
    }

    /// Resolve a symbol by walking up the scope chain
    pub fn resolve_symbol(&self, name: &str) -> Option<&Symbol> {
        self.resolve_symbol_in_scope(name, self.current_scope)
    }

    /// Resolve a symbol starting from a specific scope
    pub fn resolve_symbol_in_scope(&self, name: &str, scope_id: ScopeId) -> Option<&Symbol> {
        if let Some(scope) = self.scopes.get(&scope_id) {
            // Check current scope first
            if let Some(symbol) = scope.get_symbol(name) {
                return Some(symbol);
            }

            // Check parent scope
            if let Some(parent_id) = scope.parent {
                return self.resolve_symbol_in_scope(name, parent_id);
            }
        }

        None
    }

    /// Mark a symbol as used
    pub fn mark_symbol_used(&mut self, name: &str) -> bool {
        self.mark_symbol_used_in_scope(name, self.current_scope)
    }

    /// Mark a symbol as used starting from a specific scope
    fn mark_symbol_used_in_scope(&mut self, name: &str, scope_id: ScopeId) -> bool {
        if let Some(scope) = self.scopes.get_mut(&scope_id) {
            // Check current scope first
            if let Some(symbol) = scope.get_symbol_mut(name) {
                symbol.mark_used();
                return true;
            }

            // Check parent scope
            if let Some(parent_id) = scope.parent {
                return self.mark_symbol_used_in_scope(name, parent_id);
            }
        }

        false
    }

    /// Get all unused symbols in all scopes
    pub fn get_unused_symbols(&self) -> Vec<&Symbol> {
        let mut unused = Vec::new();

        for scope in self.scopes.values() {
            for symbol in scope.symbols.values() {
                if !symbol.is_used {
                    unused.push(symbol);
                }
            }
        }

        unused
    }

    /// Get the scope path (for debugging)
    pub fn get_scope_path(&self, scope_id: ScopeId) -> Vec<String> {
        let mut path = Vec::new();
        let mut current_id = Some(scope_id);

        while let Some(id) = current_id {
            if let Some(scope) = self.scopes.get(&id) {
                let name = match &scope.kind {
                    ScopeKind::Global => "global".to_string(),
                    ScopeKind::Function { name, .. } => format!("fn:{}", name),
                    ScopeKind::Class { name } => format!("class:{}", name),
                    ScopeKind::Trait { name } => format!("trait:{}", name),
                    ScopeKind::Block => "block".to_string(),
                    ScopeKind::Impl {
                        type_name,
                        trait_name,
                    } => {
                        if let Some(trait_name) = trait_name {
                            format!("impl:{}:for:{}", trait_name, type_name)
                        } else {
                            format!("impl:{}", type_name)
                        }
                    }
                };
                path.push(name);
                current_id = scope.parent;
            } else {
                break;
            }
        }

        path.reverse();
        path
    }
}

impl Default for ScopeManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::SymbolKind;

    use super::*;
    use bolt_ast::Visibility;
    use bolt_lexer::Position;

    #[test]
    fn test_scope_creation() {
        let mut manager = ScopeManager::new();

        // Create function scope
        let fn_scope = manager.create_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                is_async: false,
            },
            Span::new(Position::default(), Position::default()),
        );

        assert_eq!(fn_scope, ScopeId(1));
        assert_eq!(manager.current_scope(), ScopeId(0)); // Still in global

        manager.enter_scope(fn_scope);
        assert_eq!(manager.current_scope(), fn_scope);

        manager.exit_scope();
        assert_eq!(manager.current_scope(), ScopeId(0)); // Back to global
    }

    #[test]
    fn test_symbol_resolution() {
        let mut manager = ScopeManager::new();

        // Add global symbol
        let global_symbol = Symbol::new(
            "global_var".to_string(),
            SymbolKind::Variable {
                mutable: false,
                var_type: bolt_ast::Type::Int,
            },
            Visibility::Public,
            Span::default(),
            manager.global_scope(),
        );

        manager.add_symbol(global_symbol).unwrap();

        // Create function scope
        let fn_scope = manager.create_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                is_async: false,
            },
            Span::default(),
        );

        manager.enter_scope(fn_scope);

        // Should resolve global symbol from function scope
        assert!(manager.resolve_symbol("global_var").is_some());

        // Add local symbol
        let local_symbol = Symbol::new(
            "local_var".to_string(),
            SymbolKind::Variable {
                mutable: true,
                var_type: bolt_ast::Type::String,
            },
            Visibility::Private,
            Span::default(),
            fn_scope,
        );

        manager.add_symbol(local_symbol).unwrap();

        // Should resolve both symbols
        assert!(manager.resolve_symbol("global_var").is_some());
        assert!(manager.resolve_symbol("local_var").is_some());

        manager.exit_scope();

        // Back in global scope - should only resolve global symbol
        assert!(manager.resolve_symbol("global_var").is_some());
        assert!(manager.resolve_symbol("local_var").is_none());
    }
}

