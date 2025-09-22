use bolt_lexer::{LexError, Span, Token, TokenType};
use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

/// Parser errors with detailed context and recovery suggestions
#[derive(Debug, Error, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    /// Lexical error from the lexer
    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),

    /// Unexpected token found
    #[error("Unexpected token '{found}' at line {line}, column {column}")]
    UnexpectedToken {
        found: TokenType,
        expected: Vec<TokenType>,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Expected token not found
    #[error("Expected {expected} but found '{found}' at line {line}, column {column}")]
    ExpectedToken {
        expected: String,
        found: TokenType,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Unexpected end of file
    #[error("Unexpected end of file, expected {expected}")]
    UnexpectedEof { expected: String, span: Span },

    /// Invalid expression syntax
    #[error("Invalid expression syntax at line {line}, column {column}: {message}")]
    InvalidExpression {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid statement syntax
    #[error("Invalid statement syntax at line {line}, column {column}: {message}")]
    InvalidStatement {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid type annotation
    #[error("Invalid type annotation at line {line}, column {column}: {message}")]
    InvalidType {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Missing block terminator
    #[error("Missing 'end' to close block started at line {start_line}, column {start_column}")]
    MissingBlockEnd {
        start_span: Span,
        current_span: Span,
        start_line: usize,
        start_column: usize,
    },

    /// Invalid operator usage
    #[error("Invalid operator '{operator}' at line {line}, column {column}: {message}")]
    InvalidOperator {
        operator: String,
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Missing function body
    #[error("Function '{name}' is missing a body at line {line}, column {column}")]
    MissingFunctionBody {
        name: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid function parameter
    #[error("Invalid function parameter at line {line}, column {column}: {message}")]
    InvalidParameter {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid visibility modifier
    #[error("Invalid visibility modifier at line {line}, column {column}: {message}")]
    InvalidVisibility {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Mismatched delimiters
    #[error(
        "Mismatched delimiter: expected '{expected}' but found '{found}' at line {line}, column {column}"
    )]
    MismatchedDelimiter {
        expected: char,
        found: char,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid pattern in match expression
    #[error("Invalid pattern in match expression at line {line}, column {column}: {message}")]
    InvalidPattern {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid string interpolation
    #[error("Invalid string interpolation at line {line}, column {column}: {message}")]
    InvalidStringInterpolation {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Type mismatch during parsing (for contexts where type info is available)
    #[error("Type mismatch at line {line}, column {column}: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid generic parameter
    #[error("Invalid generic parameter at line {line}, column {column}: {message}")]
    InvalidGeneric {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Invalid import statement
    #[error("Invalid import statement at line {line}, column {column}: {message}")]
    InvalidImport {
        message: String,
        span: Span,
        line: usize,
        column: usize,
    },

    /// Parser internal error (should not happen in normal usage)
    #[error("Internal parser error: {message}")]
    Internal { message: String },
}

/// Parser result type
pub type ParseResult<T> = Result<T, ParseError>;

/// Error recovery suggestion
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RecoverySuggestion {
    /// Insert a missing token
    InsertToken(TokenType),
    /// Remove an unexpected token
    RemoveToken,
    /// Replace token with suggested token
    ReplaceToken(TokenType),
    /// Add missing 'end' keyword
    AddEnd,
    /// Add missing closing delimiter
    AddClosingDelimiter(char),
    /// Suggest a different keyword or identifier
    SuggestAlternative(String),
}

/// Detailed error with recovery suggestions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetailedError {
    pub error: ParseError,
    pub suggestions: Vec<RecoverySuggestion>,
    pub context: String,
}

impl ParseError {
    /// Create an unexpected token error
    pub fn unexpected_token(token: &Token, expected: Vec<TokenType>) -> Self {
        Self::UnexpectedToken {
            found: token.token_type.clone(),
            expected,
            span: token.span,
            line: token.span.start.line,
            column: token.span.start.column,
        }
    }

    /// Create an expected token error
    pub fn expected_token(expected: &str, found: &Token) -> Self {
        Self::ExpectedToken {
            expected: expected.to_string(),
            found: found.token_type.clone(),
            span: found.span,
            line: found.span.start.line,
            column: found.span.start.column,
        }
    }

    /// Create an unexpected EOF error
    pub fn unexpected_eof(expected: &str, span: Span) -> Self {
        Self::UnexpectedEof {
            expected: expected.to_string(),
            span,
        }
    }

    /// Create an invalid expression error
    pub fn invalid_expression(message: &str, span: Span) -> Self {
        Self::InvalidExpression {
            message: message.to_string(),
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Create an invalid statement error
    pub fn invalid_statement(message: &str, span: Span) -> Self {
        Self::InvalidStatement {
            message: message.to_string(),
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Create an invalid type error
    pub fn invalid_type(message: &str, span: Span) -> Self {
        Self::InvalidType {
            message: message.to_string(),
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Create a missing block end error
    pub fn missing_block_end(start_span: Span, current_span: Span) -> Self {
        Self::MissingBlockEnd {
            start_span,
            current_span,
            start_line: start_span.start.line,
            start_column: start_span.start.column,
        }
    }

    /// Create an invalid operator error
    pub fn invalid_operator(operator: &str, message: &str, span: Span) -> Self {
        Self::InvalidOperator {
            operator: operator.to_string(),
            message: message.to_string(),
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Create a missing function body error
    pub fn missing_function_body(name: &str, span: Span) -> Self {
        Self::MissingFunctionBody {
            name: name.to_string(),
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Create an invalid parameter error
    pub fn invalid_parameter(message: &str, span: Span) -> Self {
        Self::InvalidParameter {
            message: message.to_string(),
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Create a mismatched delimiter error
    pub fn mismatched_delimiter(expected: char, found: char, span: Span) -> Self {
        Self::MismatchedDelimiter {
            expected,
            found,
            span,
            line: span.start.line,
            column: span.start.column,
        }
    }

    /// Get the span associated with this error
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::LexError(_) => None,
            ParseError::UnexpectedToken { span, .. } => Some(*span),
            ParseError::ExpectedToken { span, .. } => Some(*span),
            ParseError::UnexpectedEof { span, .. } => Some(*span),
            ParseError::InvalidExpression { span, .. } => Some(*span),
            ParseError::InvalidStatement { span, .. } => Some(*span),
            ParseError::InvalidType { span, .. } => Some(*span),
            ParseError::MissingBlockEnd { current_span, .. } => Some(*current_span),
            ParseError::InvalidOperator { span, .. } => Some(*span),
            ParseError::MissingFunctionBody { span, .. } => Some(*span),
            ParseError::InvalidParameter { span, .. } => Some(*span),
            ParseError::InvalidVisibility { span, .. } => Some(*span),
            ParseError::MismatchedDelimiter { span, .. } => Some(*span),
            ParseError::InvalidPattern { span, .. } => Some(*span),
            ParseError::InvalidStringInterpolation { span, .. } => Some(*span),
            ParseError::TypeMismatch { span, .. } => Some(*span),
            ParseError::InvalidGeneric { span, .. } => Some(*span),
            ParseError::InvalidImport { span, .. } => Some(*span),
            ParseError::Internal { .. } => None,
        }
    }

    /// Get suggested recovery actions for this error
    pub fn recovery_suggestions(&self) -> Vec<RecoverySuggestion> {
        match self {
            ParseError::UnexpectedToken {
                expected, found, ..
            } => {
                let mut suggestions = vec![];

                // If we expected specific tokens, suggest inserting them
                for token_type in expected {
                    suggestions.push(RecoverySuggestion::InsertToken(token_type.clone()));
                }

                // Common recovery patterns
                match found {
                    TokenType::LeftBrace => suggestions.push(RecoverySuggestion::AddEnd),
                    TokenType::LeftParen => {
                        suggestions.push(RecoverySuggestion::AddClosingDelimiter(')'))
                    }
                    TokenType::LeftBracket => {
                        suggestions.push(RecoverySuggestion::AddClosingDelimiter(']'))
                    }
                    _ => suggestions.push(RecoverySuggestion::RemoveToken),
                }

                suggestions
            }
            ParseError::ExpectedToken { expected, .. } => {
                let mut suggestions = vec![];

                // Try to suggest the expected token type
                if expected == "end" {
                    suggestions.push(RecoverySuggestion::AddEnd);
                } else if expected.contains("closing") {
                    if expected.contains("')'") {
                        suggestions.push(RecoverySuggestion::AddClosingDelimiter(')'));
                    } else if expected.contains("']'") {
                        suggestions.push(RecoverySuggestion::AddClosingDelimiter(']'));
                    } else if expected.contains("'}'") {
                        suggestions.push(RecoverySuggestion::AddClosingDelimiter('}'));
                    }
                }

                suggestions
            }
            ParseError::MissingBlockEnd { .. } => {
                vec![RecoverySuggestion::AddEnd]
            }
            ParseError::MismatchedDelimiter { expected, .. } => {
                vec![RecoverySuggestion::AddClosingDelimiter(*expected)]
            }
            _ => vec![],
        }
    }

    /// Convert to a detailed error with context
    pub fn with_context(self, context: &str) -> DetailedError {
        let suggestions = self.recovery_suggestions();
        DetailedError {
            error: self,
            suggestions,
            context: context.to_string(),
        }
    }
}

impl DetailedError {
    /// Format this error with recovery suggestions
    pub fn format_with_suggestions(&self) -> String {
        let mut result = format!("{}", self.error);

        if !self.context.is_empty() {
            result.push_str(&format!("\n  Context: {}", self.context));
        }

        if !self.suggestions.is_empty() {
            result.push_str("\n  Suggestions:");
            for suggestion in &self.suggestions {
                result.push_str(&format!("\n    - {}", suggestion));
            }
        }

        result
    }
}

impl fmt::Display for RecoverySuggestion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecoverySuggestion::InsertToken(token) => {
                write!(f, "Insert '{}'", token)
            }
            RecoverySuggestion::RemoveToken => {
                write!(f, "Remove this token")
            }
            RecoverySuggestion::ReplaceToken(token) => {
                write!(f, "Replace with '{}'", token)
            }
            RecoverySuggestion::AddEnd => {
                write!(f, "Add 'end' to close the block")
            }
            RecoverySuggestion::AddClosingDelimiter(delimiter) => {
                write!(f, "Add closing '{}'", delimiter)
            }
            RecoverySuggestion::SuggestAlternative(alternative) => {
                write!(f, "Did you mean '{}'?", alternative)
            }
        }
    }
}

/// Helper trait for converting nom errors
pub trait IntoParseError<T> {
    fn into_parse_error(self) -> ParseResult<T>;
}

impl<T> IntoParseError<T> for nom::IResult<&[Token], T, ParseError> {
    fn into_parse_error(self) -> ParseResult<T> {
        match self {
            Ok((_, result)) => Ok(result),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Err(e),
            Err(nom::Err::Incomplete(_)) => Err(ParseError::Internal {
                message: "Incomplete input - this should not happen with token parsing".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bolt_lexer::{Position, Span};

    fn dummy_span() -> Span {
        Span::single(Position::new(1, 1, 0))
    }

    fn dummy_token() -> Token {
        Token::new(TokenType::Let, dummy_span(), "let".to_string())
    }

    #[test]
    fn test_unexpected_token_error() {
        let token = dummy_token();
        let error = ParseError::unexpected_token(&token, vec![TokenType::Fn, TokenType::Class]);

        assert!(matches!(error, ParseError::UnexpectedToken { .. }));
        assert_eq!(error.span(), Some(dummy_span()));
    }

    #[test]
    fn test_expected_token_error() {
        let token = dummy_token();
        let error = ParseError::expected_token("identifier", &token);

        assert!(matches!(error, ParseError::ExpectedToken { .. }));
        assert_eq!(error.span(), Some(dummy_span()));
    }

    #[test]
    fn test_recovery_suggestions() {
        let token = dummy_token();
        let error = ParseError::unexpected_token(&token, vec![TokenType::End]);
        let suggestions = error.recovery_suggestions();

        assert!(!suggestions.is_empty());
        assert!(suggestions.contains(&RecoverySuggestion::InsertToken(TokenType::End)));
    }

    #[test]
    fn test_detailed_error() {
        let error = ParseError::invalid_expression("test error", dummy_span());
        let detailed = error.with_context("parsing function body");

        assert_eq!(detailed.context, "parsing function body");
        assert!(
            detailed
                .format_with_suggestions()
                .contains("Context: parsing function body")
        );
    }

    #[test]
    fn test_mismatched_delimiter() {
        let error = ParseError::mismatched_delimiter(')', '}', dummy_span());
        let suggestions = error.recovery_suggestions();

        assert!(suggestions.contains(&RecoverySuggestion::AddClosingDelimiter(')')));
    }

    #[test]
    fn test_missing_block_end() {
        let start_span = dummy_span();
        let current_span = Span::single(Position::new(5, 1, 50));
        let error = ParseError::missing_block_end(start_span, current_span);
        let suggestions = error.recovery_suggestions();

        assert!(suggestions.contains(&RecoverySuggestion::AddEnd));
    }

    #[test]
    fn test_error_display() {
        let error = ParseError::invalid_expression("unexpected operator", dummy_span());
        let error_string = format!("{}", error);

        assert!(error_string.contains("Invalid expression syntax"));
        assert!(error_string.contains("unexpected operator"));
        assert!(error_string.contains("line 1, column 1"));
    }
}
