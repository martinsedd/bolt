use crate::types::{Type, TypeAnnotation};
use bolt_lexer::Span;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    /// Literal values
    Literal {
        value: LiteralValue,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Variable reference
    Identifier {
        name: String,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Binary operations: +, -, *, /, etc.
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: BinaryOp,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Unary operations: -, not, etc.
    Unary {
        operator: UnaryOp,
        operand: Box<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Function call: foo(arg1, arg2)
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Method call: obj.method(args)
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Field access: obj.field
    FieldAccess {
        object: Box<Expr>,
        field: String,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Array/map indexing: arr[index]
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Array literal: [1, 2, 3]
    Array {
        elements: Vec<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Map literal: {"key": "value"}
    Map {
        pairs: Vec<(Expr, Expr)>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// String interpolation: "Hello, {name}!"
    StringInterpolation {
        parts: Vec<StringPart>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Lambda expression: fn(x): x * 2
    Lambda {
        params: Vec<Parameter>,
        body: Box<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// If expression: if cond: then_expr else: else_expr
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Match expression
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Loop expression
    Loop {
        kind: LoopKind,
        body: Box<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Block expression: { expr1; expr2; final_expr }
    Block {
        statements: Vec<Box<Expr>>, // In Bolt, statements are expressions
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Assignment: var = value
    Assignment {
        target: Box<Expr>,
        value: Box<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Monadic operation: expr?
    Try {
        expr: Box<Expr>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Self reference: $field or $method()
    SelfRef {
        field: Option<String>,
        type_annotation: TypeAnnotation,
        span: Span,
    },

    /// Return expression: return value or => value
    Return {
        value: Option<Box<Expr>>,
        type_annotation: TypeAnnotation,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,

    // Comparison (not: .eq() and .is() are method calls)
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // Logical
    And,
    Or,
    Xor,

    // String concatenation
    Concat,

    // Assignment operations
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssing,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Minus,
    Not,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopKind {
    Infinite,
    While(Box<Expr>),
    For {
        init: Option<Box<Expr>>,
        condition: Option<Box<Expr>>,
        increment: Option<Box<Expr>>,
    },
    ForEach {
        variable: String,
        iterable: Box<Expr>,
    },
}

/// Function parameter
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub span: Span,
}

/// Match arm: pattern => expression
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
    pub span: Span,
}

/// Patter matching patterns
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard(Span),
    Literal {
        value: LiteralValue,
        span: Span,
    },
    Variable {
        name: String,
        span: Span,
    },
    Constructor {
        name: String,
        args: Vec<Pattern>,
        span: Span,
    },
}

/// String interpolation parts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StringPart {
    Text(String),
    Expression(Box<Expr>),
}

impl Expr {
    /// Get the type annotation for this expression
    pub fn type_annotation(&self) -> &TypeAnnotation {
        match self {
            Expr::Literal {
                type_annotation, ..
            } => type_annotation,
            Expr::Identifier {
                type_annotation, ..
            } => type_annotation,
            Expr::Binary {
                type_annotation, ..
            } => type_annotation,
            Expr::Unary {
                type_annotation, ..
            } => type_annotation,
            Expr::Call {
                type_annotation, ..
            } => type_annotation,
            Expr::MethodCall {
                type_annotation, ..
            } => type_annotation,
            Expr::FieldAccess {
                type_annotation, ..
            } => type_annotation,
            Expr::Index {
                type_annotation, ..
            } => type_annotation,
            Expr::Array {
                type_annotation, ..
            } => type_annotation,
            Expr::Map {
                type_annotation, ..
            } => type_annotation,
            Expr::StringInterpolation {
                type_annotation, ..
            } => type_annotation,
            Expr::Lambda {
                type_annotation, ..
            } => type_annotation,
            Expr::If {
                type_annotation, ..
            } => type_annotation,
            Expr::Match {
                type_annotation, ..
            } => type_annotation,
            Expr::Loop {
                type_annotation, ..
            } => type_annotation,
            Expr::Block {
                type_annotation, ..
            } => type_annotation,
            Expr::Assignment {
                type_annotation, ..
            } => type_annotation,
            Expr::Try {
                type_annotation, ..
            } => type_annotation,
            Expr::SelfRef {
                type_annotation, ..
            } => type_annotation,
            Expr::Return {
                type_annotation, ..
            } => type_annotation,
        }
    }

    /// Get the span for this expression
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal { span, .. } => *span,
            Expr::Identifier { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::MethodCall { span, .. } => *span,
            Expr::FieldAccess { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Array { span, .. } => *span,
            Expr::Map { span, .. } => *span,
            Expr::StringInterpolation { span, .. } => *span,
            Expr::Lambda { span, .. } => *span,
            Expr::If { span, .. } => *span,
            Expr::Match { span, .. } => *span,
            Expr::Loop { span, .. } => *span,
            Expr::Block { span, .. } => *span,
            Expr::Assignment { span, .. } => *span,
            Expr::Try { span, .. } => *span,
            Expr::SelfRef { span, .. } => *span,
            Expr::Return { span, .. } => *span,
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Expr::Literal { .. })
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Expr::Identifier { .. })
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            Expr::Literal { .. } | Expr::Identifier { .. } => false,
            Expr::Call { .. } | Expr::MethodCall { .. } => true, // Function calls may have side effects
            Expr::Assignment { .. } => true,
            Expr::Loop { .. } => true, // Loops may have side effects
            Expr::Binary { left, right, .. } => left.has_side_effects() || right.has_side_effects(),
            Expr::Unary { operand, .. } => operand.has_side_effects(),
            Expr::FieldAccess { object, .. } => object.has_side_effects(),
            Expr::Index { object, index, .. } => {
                object.has_side_effects() || index.has_side_effects()
            }
            Expr::Array { elements, .. } => elements.iter().any(|e| e.has_side_effects()),
            Expr::Map { pairs, .. } => pairs
                .iter()
                .any(|(k, v)| k.has_side_effects() || v.has_side_effects()),
            Expr::StringInterpolation { parts, .. } => parts.iter().any(|part| match part {
                StringPart::Text(_) => false,
                StringPart::Expression(expr) => expr.has_side_effects(),
            }),
            Expr::Lambda { .. } => false, // Lambda creation has no side effects
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                condition.has_side_effects()
                    || then_branch.has_side_effects()
                    || else_branch.as_ref().map_or(false, |e| e.has_side_effects())
            }
            Expr::Match { expr, arms, .. } => {
                expr.has_side_effects()
                    || arms.iter().any(|arm| {
                        arm.guard.as_ref().map_or(false, |g| g.has_side_effects())
                            || arm.body.has_side_effects()
                    })
            }
            Expr::Block { statements, .. } => statements.iter().any(|s| s.has_side_effects()),
            Expr::Try { expr, .. } => expr.has_side_effects(),
            Expr::SelfRef { .. } => false,
            Expr::Return { value, .. } => {
                true // Return always has side effects
                    || value.as_ref().map_or(false, |v| v.has_side_effects())
            }
        }
    }
}

impl LiteralValue {
    pub fn get_type(&self) -> Type {
        match self {
            LiteralValue::Integer(_) => Type::Int,
            LiteralValue::Float(_) => Type::Float,
            LiteralValue::String(_) => Type::String,
            LiteralValue::Boolean(_) => Type::Bool,
            LiteralValue::Unit => Type::Unit,
        }
    }
}

impl BinaryOp {
    pub fn result_type(&self, left: &Type, right: &Type) -> Option<Type> {
        match self {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                match (left, right) {
                    (Type::Int, Type::Int) => Some(Type::Int),
                    (Type::Float, Type::Float) => Some(Type::Float),
                    (Type::Int, Type::Float) | (Type::Float, Type::Int) => Some(Type::Float),
                    _ => None,
                }
            }

            BinaryOp::Modulo | BinaryOp::Power => {
                if matches!((left, right), (Type::Int, Type::Int)) {
                    Some(Type::Int)
                } else {
                    None
                }
            }

            BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => {
                Some(Type::Bool)
            }
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                if matches!((left, right), (Type::Bool, Type::Bool)) {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            BinaryOp::Concat => {
                if matches!((left, right), (Type::String, Type::String)) {
                    Some(Type::String)
                } else {
                    None
                }
            }
            BinaryOp::AddAssign
            | BinaryOp::SubtractAssign
            | BinaryOp::MultiplyAssign
            | BinaryOp::DivideAssing => Some(Type::Unit), // Assignment returns Unit
        }
    }
}

impl UnaryOp {
    pub fn result_type(&self, operand: &Type) -> Option<Type> {
        match self {
            UnaryOp::Minus => match operand {
                Type::Int => Some(Type::Int),
                Type::Float => Some(Type::Float),
                _ => None,
            },
            UnaryOp::Not => {
                if matches!(operand, Type::Bool) {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bolt_lexer::Position;

    fn dummy_span() -> Span {
        Span::single(Position::new(1, 1, 0))
    }

    fn dummy_type_annotation() -> TypeAnnotation {
        TypeAnnotation::inferred(dummy_span())
    }

    #[test]
    fn test_literal_expressions() {
        let int_lit = Expr::Literal {
            value: LiteralValue::Integer(42),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        assert!(int_lit.is_literal());
        assert!(!int_lit.has_side_effects());
        assert_eq!(
            int_lit.type_annotation().current_type(),
            None // No type inferred yet
        );
    }

    #[test]
    fn test_identifier_expressions() {
        let ident = Expr::Identifier {
            name: "variable".to_string(),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        assert!(ident.is_identifier());
        assert!(!ident.has_side_effects());
    }

    #[test]
    fn test_binary_expressions() {
        let left = Box::new(Expr::Literal {
            value: LiteralValue::Integer(1),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        });

        let right = Box::new(Expr::Literal {
            value: LiteralValue::Integer(2),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        });

        let binary = Expr::Binary {
            left,
            operator: BinaryOp::Add,
            right,
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        assert!(!binary.is_literal());
        assert!(!binary.has_side_effects()); // Pure arithmetic has no side effects
    }

    #[test]
    fn test_call_expressions() {
        let callee = Box::new(Expr::Identifier {
            name: "function".to_string(),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        });

        let call = Expr::Call {
            callee,
            args: vec![],
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        assert!(call.has_side_effects()); // Function calls may have side effects
    }

    #[test]
    fn test_literal_value_types() {
        assert_eq!(LiteralValue::Integer(42).get_type(), Type::Int);
        assert_eq!(LiteralValue::Float(3.14).get_type(), Type::Float);
        assert_eq!(
            LiteralValue::String("hello".to_string()).get_type(),
            Type::String
        );
        assert_eq!(LiteralValue::Boolean(true).get_type(), Type::Bool);
        assert_eq!(LiteralValue::Unit.get_type(), Type::Unit);
    }

    #[test]
    fn test_binary_op_result_types() {
        assert_eq!(
            BinaryOp::Add.result_type(&Type::Int, &Type::Int),
            Some(Type::Int)
        );
        assert_eq!(
            BinaryOp::Add.result_type(&Type::Float, &Type::Float),
            Some(Type::Float)
        );
        assert_eq!(
            BinaryOp::Add.result_type(&Type::Int, &Type::Float),
            Some(Type::Float)
        );
        assert_eq!(
            BinaryOp::Less.result_type(&Type::Int, &Type::Int),
            Some(Type::Bool)
        );
        assert_eq!(BinaryOp::Add.result_type(&Type::String, &Type::Int), None);
    }

    #[test]
    fn test_unary_op_result_types() {
        assert_eq!(UnaryOp::Minus.result_type(&Type::Int), Some(Type::Int));
        assert_eq!(UnaryOp::Minus.result_type(&Type::Float), Some(Type::Float));
        assert_eq!(UnaryOp::Not.result_type(&Type::Bool), Some(Type::Bool));
        assert_eq!(UnaryOp::Minus.result_type(&Type::String), None);
    }

    #[test]
    fn test_assignment_has_side_effects() {
        let target = Box::new(Expr::Identifier {
            name: "x".to_string(),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        });

        let value = Box::new(Expr::Literal {
            value: LiteralValue::Integer(42),
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        });

        let assignment = Expr::Assignment {
            target,
            value,
            type_annotation: dummy_type_annotation(),
            span: dummy_span(),
        };

        assert!(assignment.has_side_effects());
    }
}
