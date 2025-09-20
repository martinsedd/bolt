use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Lexer errors
#[derive(Debug, Error, Clone, PartialEq, Serialize, Deserialize)]
pub enum LexError {
    #[error("Unexpected character '{0}' at line {1}, column {2}")]
    UnexpectedCharacter(char, usize, usize),

    #[error("Unterminated string literal at line {0}, column {1}")]
    UnterminatedString(usize, usize),

    #[error("Unterminated block comment at line {0}, column {1}")]
    UnterminatedBlockComment(usize, usize),

    #[error("Invalid number format '{0}' at line {1}, column {2}")]
    InvalidNumber(String, usize, usize),

    #[error("Invalid escape sequence '\\{0}' at line {1}, column {2}")]
    InvalidEscapeSequence(char, usize, usize),

    #[error("Empty character literal at line {0}, column {1}")]
    EmptyCharacterLiteral(usize, usize),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let error = LexError::UnexpectedCharacter('@', 5, 10);
        assert_eq!(
            format!("{}", error),
            "Unexpected character '@' at line 5, column 10"
        );
    }

    #[test]
    fn test_error_equality() {
        let error1 = LexError::UnterminatedString(1, 15);
        let error2 = LexError::UnterminatedString(1, 15);
        let error3 = LexError::UnterminatedString(2, 15);

        assert_eq!(error1, error2);
        assert_ne!(error1, error3);
    }
}
