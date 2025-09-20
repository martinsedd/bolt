//! Bolt Programming Language Lexer
//!
//! This crate provides lexical analysis for the Bolt programming language,
//! converting source code into a stream of tokens for parsing.

mod error;
mod lexer;
mod position;
mod token;

// Re-export public API
pub use error::LexError;
pub use lexer::Lexer;
pub use position::{Position, Span};
pub use token::{Token, TokenType, keyword_from_str};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_public_api() {
        // Test that all public items are accessible
        let pos = Position::default();
        let span = Span::single(pos);
        let token = Token::new(TokenType::Let, span, "let".to_string());

        assert_eq!(token.token_type, TokenType::Let);
        assert!(keyword_from_str("let").is_some());
    }

    #[test]
    fn test_lexer_creation() {
        let source = "let x = 42";
        let _lexer = Lexer::new(source);
        // Just test that we can create a lexer
    }
}

