use bolt_lexer::Span;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    /// Primitive types
    Int,
    Float,
    Bool,
    String,

    /// Unit type (empty tuple)
    Unit,

    /// Array type [T]
    Array(Box<Type>),

    /// Map type {K: V}
    Map(Box<Type>, Box<Type>),

    /// Function type (param_types) -> return_type
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },

    /// User-defined types (classes, structs, enums, traits)
    UserDefined {
        name: String,
        generics: Vec<Type>,
    },

    /// Generic type parameter
    Generic {
        name: String,
        bounds: Vec<TraitBound>,
    },

    /// Reference types
    Reference {
        mutable: bool,
        inner: Box<Type>,
    },

    /// Result type Result<T, E>
    Result(Box<Type>, Box<Type>),

    /// Maybe type Maybe<T>
    Maybe(Box<Type>),

    /// Type that couldn't be inferred yet
    Unknown,

    /// Type inference variable (used during type checking)
    Infer(u32),
}

/// Trait bounds for generic types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraitBound {
    pub name: String,
    pub span: Span,
}

/// Type annotation with optional explicit type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeAnnotation {
    pub explicit_type: Option<Type>,
    pub inferred_type: Option<Type>,
    pub span: Span,
}

impl TypeAnnotation {
    pub fn new(explicit_type: Option<Type>, span: Span) -> Self {
        Self {
            explicit_type,
            inferred_type: None,
            span,
        }
    }

    pub fn explicit(typ: Type, span: Span) -> Self {
        Self {
            explicit_type: Some(typ),
            inferred_type: None,
            span,
        }
    }

    pub fn inferred(span: Span) -> Self {
        Self {
            explicit_type: None,
            inferred_type: None,
            span,
        }
    }

    /// Get the current type (explicit or inferred)
    pub fn current_type(&self) -> Option<&Type> {
        self.explicit_type.as_ref().or(self.inferred_type.as_ref())
    }

    /// Set the inferred type
    pub fn set_inferred(&mut self, typ: Type) {
        self.inferred_type = Some(typ);
    }
}

/// Generic parameter definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericParam {
    pub name: String,
    pub bounds: Vec<TraitBound>,
    pub span: Span,
}

impl Type {
    /// Check if this type is a primitive type
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Unit
        )
    }

    /// Check if this type is a collection type
    pub fn is_collection(&self) -> bool {
        matches!(self, Type::Array(_) | Type::Map(_, _))
    }

    /// Check if this type is a reference type
    pub fn is_reference(&self) -> bool {
        matches!(self, Type::Reference { .. })
    }

    /// Check if this type is a function type
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }

    /// Check if this type needs generic resolution
    pub fn has_generics(&self) -> bool {
        match self {
            Type::Generic { .. } => true,
            Type::UserDefined { generics, .. } => !generics.is_empty(),
            Type::Array(inner) => inner.has_generics(),
            Type::Map(k, v) => k.has_generics() || v.has_generics(),
            Type::Function {
                params,
                return_type,
            } => params.iter().any(|p| p.has_generics()) || return_type.has_generics(),
            Type::Reference { inner, .. } => inner.has_generics(),
            Type::Result(ok, err) => ok.has_generics() || err.has_generics(),
            Type::Maybe(inner) => inner.has_generics(),
            _ => false,
        }
    }

    /// Get the inner type for reference types
    pub fn deref_type(&self) -> Option<&Type> {
        match self {
            Type::Reference { inner, .. } => Some(inner),
            _ => None,
        }
    }

    /// Create a mutable reference to this type
    pub fn as_mut_ref(&self) -> Type {
        Type::Reference {
            mutable: true,
            inner: Box::new(self.clone()),
        }
    }

    /// Create an immutable reference to this type
    pub fn as_ref(&self) -> Type {
        Type::Reference {
            mutable: false,
            inner: Box::new(self.clone()),
        }
    }

    pub fn substitute(&self, substitutions: &HashMap<String, Type>) -> Type {
        match self {
            Type::Generic { name, .. } => substitutions
                .get(name)
                .cloned()
                .unwrap_or_else(|| self.clone()),
            Type::Array(inner) => Type::Array(Box::new(inner.substitute(substitutions))),
            Type::Map(k, v) => Type::Map(
                Box::new(k.substitute(substitutions)),
                Box::new(v.substitute(substitutions)),
            ),
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params.iter().map(|p| p.substitute(substitutions)).collect(),
                return_type: Box::new(return_type.substitute(substitutions)),
            },
            Type::UserDefined { name, generics } => Type::UserDefined {
                name: name.clone(),
                generics: generics
                    .iter()
                    .map(|g| g.substitute(substitutions))
                    .collect(),
            },
            Type::Reference { mutable, inner } => Type::Reference {
                mutable: *mutable,
                inner: Box::new(inner.substitute(substitutions)),
            },
            Type::Result(ok, err) => Type::Result(
                Box::new(ok.substitute(substitutions)),
                Box::new(err.substitute(substitutions)),
            ),
            Type::Maybe(inner) => Type::Maybe(Box::new(inner.substitute(substitutions))),
            _ => self.clone(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Unit => write!(f, "()"),
            Type::Array(inner) => write!(f, "[{}]", inner),
            Type::Map(k, v) => write!(f, "{{{}: {}}}", k, v),
            Type::Function {
                params,
                return_type,
            } => {
                let param_str = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", param_str, return_type)
            }
            Type::UserDefined { name, generics } => {
                if generics.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let generic_str = generics
                        .iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}<{}>", name, generic_str)
                }
            }
            Type::Generic { name, bounds } => {
                if bounds.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let bound_str = bounds
                        .iter()
                        .map(|b| b.name.as_str())
                        .collect::<Vec<_>>()
                        .join(" + ");
                    write!(f, "{}: {}", name, bound_str)
                }
            }
            Type::Reference { mutable, inner } => {
                if *mutable {
                    write!(f, "&mut {}", inner)
                } else {
                    write!(f, "&{}", inner)
                }
            }
            Type::Result(ok, err) => write!(f, "Result<{}, {}>", ok, err),
            Type::Maybe(inner) => write!(f, "Maybe<{}>", inner),
            Type::Unknown => write!(f, "?"),
            Type::Infer(id) => write!(f, "?{}", id),
        }
    }
}

impl fmt::Display for TraitBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for GenericParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.bounds.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let bound_str = self
                .bounds
                .iter()
                .map(|b| b.name.as_str())
                .collect::<Vec<_>>()
                .join(" + ");
            write!(f, "{}: {}", self.name, bound_str)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bolt_lexer::Position;

    #[test]
    fn test_primitive_types() {
        assert!(Type::Int.is_primitive());
        assert!(Type::String.is_primitive());
        assert!(!Type::Array(Box::new(Type::Int)).is_primitive());
    }

    #[test]
    fn test_collection_types() {
        let array_type = Type::Array(Box::new(Type::Int));
        let map_type = Type::Map(Box::new(Type::String), Box::new(Type::Int));

        assert!(array_type.is_collection());
        assert!(map_type.is_collection());
        assert!(!Type::Int.is_collection());
    }

    #[test]
    fn test_reference_types() {
        let int_ref = Type::Int.as_ref();
        let int_mut_ref = Type::Int.as_mut_ref();

        assert!(int_ref.is_reference());
        assert!(int_mut_ref.is_reference());

        if let Type::Reference { mutable, inner } = int_ref {
            assert!(!mutable);
            assert_eq!(*inner, Type::Int);
        }

        if let Type::Reference { mutable, inner } = int_mut_ref {
            assert!(mutable);
            assert_eq!(*inner, Type::Int);
        }
    }

    #[test]
    fn test_function_types() {
        let func_type = Type::Function {
            params: vec![Type::Int, Type::String],
            return_type: Box::new(Type::Bool),
        };

        assert!(func_type.is_function());
        assert!(!func_type.is_primitive());
    }

    #[test]
    fn test_generic_substitution() {
        let generic_type = Type::Generic {
            name: "T".to_string(),
            bounds: vec![],
        };

        let mut substitutions = HashMap::new();
        substitutions.insert("T".to_string(), Type::Int);

        let substituted = generic_type.substitute(&substitutions);
        assert_eq!(substituted, Type::Int);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(Type::Int.to_string(), "Int");
        assert_eq!(Type::Array(Box::new(Type::String)).to_string(), "[String]");
        assert_eq!(
            Type::Map(Box::new(Type::String), Box::new(Type::Int)).to_string(),
            "{String: Int}"
        );
        assert_eq!(
            Type::Function {
                params: vec![Type::Int, Type::String],
                return_type: Box::new(Type::Bool),
            }
            .to_string(),
            "(Int, String) -> Bool"
        );
    }

    #[test]
    fn test_type_annotation() {
        let span = Span::single(Position::new(1, 1, 0));
        let mut annotation = TypeAnnotation::explicit(Type::Int, span);

        assert_eq!(annotation.current_type(), Some(&Type::Int));

        annotation.set_inferred(Type::String);
        // Explicit type takes precedence
        assert_eq!(annotation.current_type(), Some(&Type::Int));

        let mut inferred_annotation = TypeAnnotation::inferred(span);
        assert_eq!(inferred_annotation.current_type(), None);

        inferred_annotation.set_inferred(Type::Float);
        assert_eq!(inferred_annotation.current_type(), Some(&Type::Float));
    }
}
