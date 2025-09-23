use bolt_lexer::Span;
use thiserror::Error;

/// Semantic analysis errors
#[derive(Error, Debug, Clone, PartialEq)]
pub enum SemanticError {
    #[error("Undefined symbol '{name}'")]
    UndefinedSymbol { name: String, span: Span },

    #[error("Symbol '{name}' is already defined")]
    DuplicateSymbol {
        name: String,
        span: Span,
        previous_span: Span,
    },

    #[error("Cannot assign to immutable variable '{name}'")]
    AssignToImmutable { name: String, span: Span },

    #[error("Type mismatch: expected '{expected}', found '{found}'")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Cannot call '{name}' with {arg_count} arguments, expected {expected_count}")]
    ArgumentCountMismatch {
        name: String,
        arg_count: usize,
        expected_count: usize,
        span: Span,
    },

    #[error("Cannot access private symbol '{name}'")]
    PrivateAccess { name: String, span: Span },

    #[error("Cannot access '{field}' on type '{type_name}'")]
    InvalidFieldAccess {
        field: String,
        type_name: String,
        span: Span,
    },

    #[error("Cannot call method '{method}' on type '{type_name}'")]
    InvalidMethodCall {
        method: String,
        type_name: String,
        span: Span,
    },

    #[error("Return type mismatch: expected '{expected}', found '{found}'")]
    ReturnTypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Function '{name}' must return a value")]
    MissingReturn { name: String, span: Span },

    #[error("Unreachable code")]
    UnreachableCode { span: Span },

    #[error("Variable '{name}' is never used")]
    UnusedVariable { name: String, span: Span },

    #[error("Function '{name}' is never used")]
    UnusedFunction { name: String, span: Span },

    #[error("Import '{name}' is never used")]
    UnusedImport { name: String, span: Span },

    #[error("Circular dependency detected: {cycle}")]
    CircularDependency { cycle: String, span: Span },

    #[error("Cannot inherit from '{type_name}': {reason}")]
    InvalidInheritance {
        type_name: String,
        reason: String,
        span: Span,
    },

    #[error("Trait '{trait_name}' is not implemented for type '{type_name}'")]
    TraitNotImplemented {
        trait_name: String,
        type_name: String,
        span: Span,
    },

    #[error("Method '{method}' is required by trait '{trait_name}' but not implemented")]
    MissingTraitMethod {
        method: String,
        trait_name: String,
        span: Span,
    },

    #[error("Cannot override method '{method}': {reason}")]
    InvalidOverride {
        method: String,
        reason: String,
        span: Span,
    },

    #[error("Pure function '{name}' cannot have side effects")]
    ImpureFunctionCall { name: String, span: Span },

    #[error("Async function '{name}' can only be called from async context")]
    AsyncInSyncContext { name: String, span: Span },

    #[error("Borrow checker error: {message}")]
    BorrowError { message: String, span: Span },

    #[error("Ownership error: {message}")]
    OwnershipError { message: String, span: Span },

    #[error("Lifetime error: {message}")]
    LifetimeError { message: String, span: Span },
}

impl SemanticError {
    /// Get the span where this error occurred
    pub fn span(&self) -> Span {
        match self {
            Self::UndefinedSymbol { span, .. } => *span,
            Self::DuplicateSymbol { span, .. } => *span,
            Self::AssignToImmutable { span, .. } => *span,
            Self::TypeMismatch { span, .. } => *span,
            Self::ArgumentCountMismatch { span, .. } => *span,
            Self::PrivateAccess { span, .. } => *span,
            Self::InvalidFieldAccess { span, .. } => *span,
            Self::InvalidMethodCall { span, .. } => *span,
            Self::ReturnTypeMismatch { span, .. } => *span,
            Self::MissingReturn { span, .. } => *span,
            Self::UnreachableCode { span, .. } => *span,
            Self::UnusedVariable { span, .. } => *span,
            Self::UnusedFunction { span, .. } => *span,
            Self::UnusedImport { span, .. } => *span,
            Self::CircularDependency { span, .. } => *span,
            Self::InvalidInheritance { span, .. } => *span,
            Self::TraitNotImplemented { span, .. } => *span,
            Self::MissingTraitMethod { span, .. } => *span,
            Self::InvalidOverride { span, .. } => *span,
            Self::ImpureFunctionCall { span, .. } => *span,
            Self::AsyncInSyncContext { span, .. } => *span,
            Self::BorrowError { span, .. } => *span,
            Self::OwnershipError { span, .. } => *span,
            Self::LifetimeError { span, .. } => *span,
        }
    }

    /// Check if this is a warning (non-fatal error)
    pub fn is_warning(&self) -> bool {
        matches!(
            self,
            Self::UnusedVariable { .. }
                | Self::UnusedFunction { .. }
                | Self::UnusedImport { .. }
                | Self::UnreachableCode { .. }
        )
    }

    /// Get error severity
    pub fn severity(&self) -> ErrorSeverity {
        if self.is_warning() {
            ErrorSeverity::Warning
        } else {
            ErrorSeverity::Error
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    Error,
    Warning,
    Info,
}

/// Collection of semantic errors with utilities for reporting
#[derive(Debug, Clone, Default)]
pub struct SemanticErrors {
    errors: Vec<SemanticError>,
}

impl SemanticErrors {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Add an error
    pub fn add(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    /// Add multiple errors
    pub fn extend(&mut self, errors: impl IntoIterator<Item = SemanticError>) {
        self.errors.extend(errors);
    }

    /// Get all errors
    pub fn errors(&self) -> &[SemanticError] {
        &self.errors
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.errors.iter().any(|e| !e.is_warning())
    }

    /// Check if there are any warnings
    pub fn has_warnings(&self) -> bool {
        self.errors.iter().any(|e| e.is_warning())
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// Get count of errors (excluding warnings)
    pub fn error_count(&self) -> usize {
        self.errors.iter().filter(|e| !e.is_warning()).count()
    }

    /// Get count of warnings
    pub fn warning_count(&self) -> usize {
        self.errors.iter().filter(|e| e.is_warning()).count()
    }

    /// Get total count
    pub fn total_count(&self) -> usize {
        self.errors.len()
    }

    /// Sort errors by span (line, then column)
    pub fn sort_by_location(&mut self) {
        self.errors.sort_by_key(|e| {
            let span = e.span();
            (span.start.line, span.start.column)
        });
    }

    /// Get errors of a specific severity
    pub fn errors_with_severity(&self, severity: ErrorSeverity) -> Vec<&SemanticError> {
        self.errors
            .iter()
            .filter(|e| e.severity() == severity)
            .collect()
    }

    /// Clear all errors
    pub fn clear(&mut self) {
        self.errors.clear();
    }

    /// Create a formatted error report
    pub fn format_report(&self, source_code: &str) -> String {
        if self.errors.is_empty() {
            return "No semantic errors found.".to_string();
        }

        let mut report = String::new();

        // Summary
        let error_count = self.error_count();
        let warning_count = self.warning_count();

        if error_count > 0 {
            report.push_str(&format!("Found {} error(s)", error_count));
            if warning_count > 0 {
                report.push_str(&format!(" and {} warning(s)", warning_count));
            }
        } else {
            report.push_str(&format!("Found {} warning(s)", warning_count));
        }
        report.push_str(":\n\n");

        // Individual errors
        let lines: Vec<&str> = source_code.lines().collect();

        for (i, error) in self.errors.iter().enumerate() {
            let span = error.span();
            let severity = if error.is_warning() {
                "warning"
            } else {
                "error"
            };

            report.push_str(&format!(
                "{}[{}]: {} at line {}, column {}\n",
                i + 1,
                severity,
                error,
                span.start.line,
                span.start.column
            ));

            // Show source line if available
            if span.start.line > 0 && span.start.line <= lines.len() {
                let line = lines[span.start.line - 1];
                report.push_str(&format!("  {}\n", line));

                // Show pointer to error location
                let pointer_offset = span.start.column.saturating_sub(1);
                let pointer = " ".repeat(pointer_offset) + "^";
                report.push_str(&format!("  {}\n", pointer));
            }

            report.push('\n');
        }

        report
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bolt_lexer::Position;

    #[test]
    fn test_error_creation() {
        let span = Span::new(Position::default(), Position::default());
        let error = SemanticError::UndefinedSymbol {
            name: "test".to_string(),
            span,
        };

        assert_eq!(error.span(), span);
        assert!(!error.is_warning());
        assert_eq!(error.severity(), ErrorSeverity::Error);
    }

    #[test]
    fn test_warning_detection() {
        let span = Span::new(Position::default(), Position::default());
        let warning = SemanticError::UnusedVariable {
            name: "test".to_string(),
            span,
        };

        assert!(warning.is_warning());
        assert_eq!(warning.severity(), ErrorSeverity::Warning);
    }

    #[test]
    fn test_error_collection() {
        let mut errors = SemanticErrors::new();
        let span = Span::new(Position::default(), Position::default());

        errors.add(SemanticError::UndefinedSymbol {
            name: "test".to_string(),
            span,
        });

        errors.add(SemanticError::UnusedVariable {
            name: "unused".to_string(),
            span,
        });

        assert_eq!(errors.total_count(), 2);
        assert_eq!(errors.error_count(), 1);
        assert_eq!(errors.warning_count(), 1);
        assert!(errors.has_errors());
        assert!(errors.has_warnings());
    }

    #[test]
    fn test_error_formatting() {
        let mut errors = SemanticErrors::new();
        let span = Span::new(
            Position {
                line: 1,
                column: 5,
                offset: 4,
            },
            Position {
                line: 1,
                column: 8,
                offset: 7,
            },
        );

        errors.add(SemanticError::UndefinedSymbol {
            name: "foo".to_string(),
            span,
        });

        let source = "let x = foo;";
        let report = errors.format_report(source);

        assert!(report.contains("Undefined symbol 'foo'"));
        assert!(report.contains("line 1, column 5"));
        assert!(report.contains("let x = foo;"));
    }
}

