use crate::expr::{Expr, Parameter, Pattern};
use crate::types::{GenericParam, Type, TypeAnnotation};
use bolt_lexer::Span;
use serde::{Deserialize, Serialize};

/// Statement AST nodes
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    /// Variable declaration: let x = value or mut x :: Type = value
    VariableDecl {
        name: String,
        mutable: bool,
        type_annotation: TypeAnnotation,
        initializer: Option<Box<Expr>>,
        span: Span,
    },

    /// Function definition
    FunctionDef {
        name: String,
        is_pure: bool,
        is_async: bool,
        generics: Vec<GenericParam>,
        params: Vec<Parameter>,
        return_type: TypeAnnotation,
        body: Box<Expr>, // Function body is an expression
        visibility: Visibility,
        span: Span,
    },

    /// Class definition
    ClassDef {
        name: String,
        generics: Vec<GenericParam>,
        superclass: Option<String>,
        traits: Vec<String>, // Implemented traits
        body: ClassBody,
        visibility: Visibility,
        span: Span,
    },

    /// Struct definition
    StructDef {
        name: String,
        generics: Vec<GenericParam>,
        fields: Vec<StructField>,
        visibility: Visibility,
        span: Span,
    },

    /// Enum definition
    EnumDef {
        name: String,
        generics: Vec<GenericParam>,
        variants: Vec<EnumVariant>,
        visibility: Visibility,
        span: Span,
    },

    /// Trait definition
    TraitDef {
        name: String,
        generics: Vec<GenericParam>,
        supertraits: Vec<String>,
        body: TraitBody,
        visibility: Visibility,
        span: Span,
    },

    /// Implementation block: impl TraitName for TypeName
    ImplBlock {
        trait_name: Option<String>, // None for inherent impl
        type_name: String,
        generics: Vec<GenericParam>,
        body: Vec<ImplItem>,
        span: Span,
    },

    /// Module definition
    ModuleDef {
        name: String,
        body: Vec<Box<Stmt>>,
        visibility: Visibility,
        span: Span,
    },

    /// Import statement: import ModuleName or import ModuleName::Item
    Import {
        path: ImportPath,
        alias: Option<String>,
        visibility: Visibility, // For re-exports
        span: Span,
    },

    /// Expression statement (expression used as statement)
    ExpressionStmt { expr: Box<Expr>, span: Span },

    /// If statement (when used as statement, not expression)
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },

    /// Loop statement
    Loop {
        kind: LoopKind,
        body: Box<Stmt>,
        span: Span,
    },

    /// Match statement
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },

    /// Return statement: return expr
    Return {
        value: Option<Box<Expr>>,
        span: Span,
    },

    /// Break statement
    Break { label: Option<String>, span: Span },

    /// Continue statement
    Continue { label: Option<String>, span: Span },

    /// Defer statement: defer cleanup()
    Defer { expr: Box<Expr>, span: Span },

    /// Block statement: { statements... }
    Block {
        statements: Vec<Box<Stmt>>,
        span: Span,
    },
}

/// Visibility modifiers
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    /// Private (default)
    Private,
    /// Public: pub
    Public,
    /// Package-private: pub(package)
    Package,
    /// Module-private: pub(module)
    Module,
    /// Protected (for inheritance): protected
    Protected,
    /// Internal: internal
    Internal,
    /// Test-only: pub(test)
    Test,
    /// Friend access: pub(friend: TypeName)
    Friend(Vec<String>),
}

/// Class body contents
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassBody {
    pub fields: Vec<ClassField>,
    pub methods: Vec<ClassMethod>,
    pub constructors: Vec<Constructor>,
}

/// Class field definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassField {
    pub name: String,
    pub mutable: bool,
    pub type_annotation: TypeAnnotation,
    pub visibility: Visibility,
    pub getter: Option<AccessorDef>,
    pub setter: Option<AccessorDef>,
    pub span: Span,
}

/// Accessor definition (get/set)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AccessorDef {
    pub body: Option<Box<Expr>>, // None for auto-generated
    pub span: Span,
}

/// Class method definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassMethod {
    pub name: String,
    pub is_pure: bool,
    pub is_async: bool,
    pub is_override: bool,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Parameter>,
    pub return_type: TypeAnnotation,
    pub body: Box<Expr>,
    pub visibility: Visibility,
    pub span: Span,
}

/// Constructor definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Constructor {
    pub params: Vec<Parameter>,
    pub body: Option<Box<Expr>>, // None for auto-generated
    pub span: Span,
}

/// Struct field definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructField {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub visibility: Visibility,
    pub span: Span,
}

/// Enum variant definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    pub data: EnumVariantData,
    pub span: Span,
}

/// Enum variant data
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EnumVariantData {
    /// Unit variant: Red
    Unit,
    /// Tuple variant: RGB(Int, Int, Int)
    Tuple(Vec<Type>),
    /// Struct variant: Point { x: Int, y: Int }
    Struct(Vec<StructField>),
}

/// Trait body contents
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitBody {
    pub methods: Vec<TraitMethod>,
}

/// Trait method declaration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMethod {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Parameter>,
    pub return_type: TypeAnnotation,
    pub default_body: Option<Box<Expr>>, // Default implementation
    pub span: Span,
}

/// Implementation item
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ImplItem {
    /// Method implementation
    Method(ClassMethod),
    /// Associated type
    Type {
        name: String,
        type_def: Type,
        span: Span,
    },
    /// Associated constant
    Const {
        name: String,
        type_annotation: TypeAnnotation,
        value: Box<Expr>,
        span: Span,
    },
}

/// Import path
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ImportPath {
    /// Simple import: ModuleName
    Simple(String),
    /// Nested import: ModuleName::Item
    Nested(String, String),
    /// Glob import: ModuleName::*
    Glob(String),
    /// Multiple imports: ModuleName::{Item1, Item2}
    Multiple(String, Vec<String>),
}

/// Loop kinds for statements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopKind {
    /// Infinite loop: loop : body end
    Infinite,
    /// While loop: loop (condition) : body end
    While(Box<Expr>),
    /// For loop: loop (init; condition; increment) : body end
    For {
        init: Option<Box<Stmt>>, // Note: Stmt, not Expr for declarations
        condition: Option<Box<Expr>>,
        increment: Option<Box<Expr>>,
    },
    /// Foreach loop: loop collection : body end
    ForEach {
        variable: String,
        iterable: Box<Expr>,
    },
}

/// Match arm for statements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Stmt>, // Note: Stmt, not Expr
    pub span: Span,
}

impl Stmt {
    /// Get the span for this statement
    pub fn span(&self) -> Span {
        match self {
            Stmt::VariableDecl { span, .. } => *span,
            Stmt::FunctionDef { span, .. } => *span,
            Stmt::ClassDef { span, .. } => *span,
            Stmt::StructDef { span, .. } => *span,
            Stmt::EnumDef { span, .. } => *span,
            Stmt::TraitDef { span, .. } => *span,
            Stmt::ImplBlock { span, .. } => *span,
            Stmt::ModuleDef { span, .. } => *span,
            Stmt::Import { span, .. } => *span,
            Stmt::ExpressionStmt { span, .. } => *span,
            Stmt::If { span, .. } => *span,
            Stmt::Loop { span, .. } => *span,
            Stmt::Match { span, .. } => *span,
            Stmt::Return { span, .. } => *span,
            Stmt::Break { span, .. } => *span,
            Stmt::Continue { span, .. } => *span,
            Stmt::Defer { span, .. } => *span,
            Stmt::Block { span, .. } => *span,
        }
    }

    /// Check if this statement is a declaration
    pub fn is_declaration(&self) -> bool {
        matches!(
            self,
            Stmt::VariableDecl { .. }
                | Stmt::FunctionDef { .. }
                | Stmt::ClassDef { .. }
                | Stmt::StructDef { .. }
                | Stmt::EnumDef { .. }
                | Stmt::TraitDef { .. }
                | Stmt::ImplBlock { .. }
                | Stmt::ModuleDef { .. }
        )
    }

    /// Check if this statement is a control flow statement
    pub fn is_control_flow(&self) -> bool {
        matches!(
            self,
            Stmt::If { .. }
                | Stmt::Loop { .. }
                | Stmt::Match { .. }
                | Stmt::Return { .. }
                | Stmt::Break { .. }
                | Stmt::Continue { .. }
        )
    }

    /// Check if this statement has a body that can contain other statements
    pub fn has_body(&self) -> bool {
        matches!(
            self,
            Stmt::FunctionDef { .. }
                | Stmt::ClassDef { .. }
                | Stmt::TraitDef { .. }
                | Stmt::ImplBlock { .. }
                | Stmt::ModuleDef { .. }
                | Stmt::If { .. }
                | Stmt::Loop { .. }
                | Stmt::Match { .. }
                | Stmt::Block { .. }
        )
    }

    /// Get the name of this statement if it's a named declaration
    pub fn name(&self) -> Option<&str> {
        match self {
            Stmt::VariableDecl { name, .. } => Some(name),
            Stmt::FunctionDef { name, .. } => Some(name),
            Stmt::ClassDef { name, .. } => Some(name),
            Stmt::StructDef { name, .. } => Some(name),
            Stmt::EnumDef { name, .. } => Some(name),
            Stmt::TraitDef { name, .. } => Some(name),
            Stmt::ModuleDef { name, .. } => Some(name),
            _ => None,
        }
    }

    /// Get the visibility of this statement if it has one
    pub fn visibility(&self) -> Option<&Visibility> {
        match self {
            Stmt::FunctionDef { visibility, .. } => Some(visibility),
            Stmt::ClassDef { visibility, .. } => Some(visibility),
            Stmt::StructDef { visibility, .. } => Some(visibility),
            Stmt::EnumDef { visibility, .. } => Some(visibility),
            Stmt::TraitDef { visibility, .. } => Some(visibility),
            Stmt::ModuleDef { visibility, .. } => Some(visibility),
            Stmt::Import { visibility, .. } => Some(visibility),
            _ => None,
        }
    }
}

impl Visibility {
    /// Check if this visibility allows access from the given context
    pub fn allows_access_from(&self, context: &str) -> bool {
        match self {
            Visibility::Private => false, // Only accessible within the same scope
            Visibility::Public => true,
            Visibility::Package => true,   // TODO: Check if same package
            Visibility::Module => true,    // TODO: Check if same module
            Visibility::Protected => true, // TODO: Check if subclass
            Visibility::Internal => true,  // TODO: Check if same crate
            Visibility::Test => false,     // TODO: Check if in test context
            Visibility::Friend(friends) => friends.contains(&context.to_string()),
        }
    }

    /// Check if this is a public visibility
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }

    /// Check if this is a private visibility
    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::LiteralValue;
    use bolt_lexer::Position;

    fn dummy_span() -> Span {
        Span::single(Position::new(1, 1, 0))
    }

    fn dummy_type_annotation() -> TypeAnnotation {
        TypeAnnotation::inferred(dummy_span())
    }

    #[test]
    fn test_variable_declaration() {
        let var_decl = Stmt::VariableDecl {
            name: "x".to_string(),
            mutable: false,
            type_annotation: dummy_type_annotation(),
            initializer: Some(Box::new(Expr::Literal {
                value: LiteralValue::Integer(42),
                type_annotation: dummy_type_annotation(),
                span: dummy_span(),
            })),
            span: dummy_span(),
        };

        assert!(var_decl.is_declaration());
        assert_eq!(var_decl.name(), Some("x"));
        assert!(!var_decl.is_control_flow());
    }

    #[test]
    fn test_function_definition() {
        let func_def = Stmt::FunctionDef {
            name: "add".to_string(),
            is_pure: true,
            is_async: false,
            generics: vec![],
            params: vec![],
            return_type: TypeAnnotation::explicit(Type::Int, dummy_span()),
            body: Box::new(Expr::Literal {
                value: LiteralValue::Integer(0),
                type_annotation: dummy_type_annotation(),
                span: dummy_span(),
            }),
            visibility: Visibility::Public,
            span: dummy_span(),
        };

        assert!(func_def.is_declaration());
        assert_eq!(func_def.name(), Some("add"));
        assert!(func_def.has_body());
        assert_eq!(func_def.visibility(), Some(&Visibility::Public));
    }

    #[test]
    fn test_class_definition() {
        let class_def = Stmt::ClassDef {
            name: "User".to_string(),
            generics: vec![],
            superclass: None,
            traits: vec![],
            body: ClassBody {
                fields: vec![],
                methods: vec![],
                constructors: vec![],
            },
            visibility: Visibility::Public,
            span: dummy_span(),
        };

        assert!(class_def.is_declaration());
        assert_eq!(class_def.name(), Some("User"));
        assert!(class_def.has_body());
    }

    #[test]
    fn test_control_flow_statement() {
        let if_stmt = Stmt::If {
            condition: Box::new(Expr::Literal {
                value: LiteralValue::Boolean(true),
                type_annotation: dummy_type_annotation(),
                span: dummy_span(),
            }),
            then_branch: Box::new(Stmt::Block {
                statements: vec![],
                span: dummy_span(),
            }),
            else_branch: None,
            span: dummy_span(),
        };

        let return_stmt = Stmt::Return {
            value: None,
            span: dummy_span(),
        };

        assert!(if_stmt.is_control_flow());
        assert!(return_stmt.is_control_flow());
        assert!(!if_stmt.is_declaration());
    }
    #[test]
    fn test_visibility() {
        assert!(Visibility::Public.is_public());
        assert!(Visibility::Private.is_private());
        assert!(!Visibility::Public.is_private());

        let friend_vis = Visibility::Friend(vec!["TestClass".to_string()]);
        assert!(friend_vis.allows_access_from("TestClass"));
        assert!(!friend_vis.allows_access_from("OtherClass"));
    }

    #[test]
    fn test_import_paths() {
        let simple_import = ImportPath::Simple("Math".to_string());
        let nested_import = ImportPath::Nested("IO".to_string(), "readFile".to_string());
        let glob_import = ImportPath::Glob("Collections".to_string());
        let multiple_import = ImportPath::Multiple(
            "Utils".to_string(),
            vec!["helper1".to_string(), "helper2".to_string()],
        );

        // Just test that they can be created without panicking
        assert!(matches!(simple_import, ImportPath::Simple(_)));
        assert!(matches!(nested_import, ImportPath::Nested(_, _)));
        assert!(matches!(glob_import, ImportPath::Glob(_)));
        assert!(matches!(multiple_import, ImportPath::Multiple(_, _)));
    }

    #[test]
    fn test_enum_variants() {
        let unit_variant = EnumVariant {
            name: "Red".to_string(),
            data: EnumVariantData::Unit,
            span: dummy_span(),
        };

        let tuple_variant = EnumVariant {
            name: "RGB".to_string(),
            data: EnumVariantData::Tuple(vec![Type::Int, Type::Int, Type::Int]),
            span: dummy_span(),
        };

        assert!(matches!(unit_variant.data, EnumVariantData::Unit));
        assert!(matches!(tuple_variant.data, EnumVariantData::Tuple(_)));
    }
}
