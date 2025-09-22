use crate::error::{ParseError, ParseResult};
use bolt_ast::Stmt;
use bolt_lexer::{Position, Span, Token, TokenType};
use std::sync::LazyLock;

/// Main parser struct that manages the token stream and parsing state
#[derive(Debug, Clone)]
pub struct Parser {
    /// Token stream
    tokens: Vec<Token>,
    /// Current position in token stream
    current: usize,
    /// Previous token (for error reporting)
    previous: Option<Token>,
    /// Parser configuration
    config: ParserConfig,
    /// Error recovery state
    error_recovery: ErrorRecoveryState,
}

/// Parser configuration options
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// Allow incomplete parsing (useful for LSP)
    pub allow_incomplete: bool,
    /// Maximum number of errors before giving up
    pub max_errors: usize,
    /// Enable error recovery
    pub enable_recovery: bool,
    /// Skip comments during parsing
    pub skip_comments: bool,
    /// Skip newlines during parsing
    pub skip_newlines: bool,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            allow_incomplete: false,
            max_errors: 10,
            enable_recovery: true,
            skip_comments: true,
            skip_newlines: true,
        }
    }
}

/// Error recovery state
#[derive(Debug, Clone)]
struct ErrorRecoveryState {
    /// Collected errors during parsing
    errors: Vec<ParseError>,
    /// Whether we're currently in error recovery mode
    in_recovery: bool,
    /// Tokens to synchronize on for recovery
    sync_tokens: Vec<TokenType>,
}

impl Default for ErrorRecoveryState {
    fn default() -> Self {
        Self {
            errors: Vec::new(),
            in_recovery: false,
            sync_tokens: vec![
                TokenType::Fn,
                TokenType::Class,
                TokenType::Struct,
                TokenType::Enum,
                TokenType::Trait,
                TokenType::Impl,
                TokenType::Module,
                TokenType::Import,
                TokenType::Let,
                TokenType::If,
                TokenType::Loop,
                TokenType::Match,
                TokenType::Return,
                TokenType::End,
            ],
        }
    }
}

// Lazy static EOF token to avoid const function issues
static EOF_TOKEN: LazyLock<Token> = LazyLock::new(|| {
    Token::new(
        TokenType::Eof,
        Span::single(Position::new(0, 0, 0)),
        String::new(),
    )
});

impl Parser {
    /// Create a new parser with default configuration
    pub fn new(tokens: Vec<Token>) -> Self {
        Self::with_config(tokens, ParserConfig::default())
    }

    /// Create a new parser with custom configuration
    pub fn with_config(tokens: Vec<Token>, config: ParserConfig) -> Self {
        Self {
            tokens,
            current: 0,
            previous: None,
            config,
            error_recovery: ErrorRecoveryState::default(),
        }
    }

    /// Parse a complete program (multiple statements)
    pub fn parse_program(&mut self) -> ParseResult<Vec<Box<Stmt>>> {
        let mut statements = Vec::new();

        // Skip any leading whitespace/comments
        self.skip_trivia();

        while !self.is_at_end() {
            // Try to parse a statement
            match self.parse_statement() {
                Ok(stmt) => {
                    statements.push(Box::new(stmt));
                    self.skip_trivia();
                }
                Err(error) => {
                    // Handle error based on recovery configuration
                    if self.config.enable_recovery {
                        self.record_error(error);
                        if !self.recover_to_sync_point() {
                            break; // Couldn't recover, stop parsing
                        }
                    } else {
                        return Err(error);
                    }
                }
            }

            // Check if we've hit the error limit
            if self.error_recovery.errors.len() >= self.config.max_errors {
                break;
            }
        }

        // If we collected errors during recovery, report them
        if !self.error_recovery.errors.is_empty() && !self.config.allow_incomplete {
            return Err(self.error_recovery.errors[0].clone()); // Clone instead of moving
        }

        Ok(statements)
    }

    /// BUG:Parse a single statement
    pub fn parse_statement(&mut self) -> ParseResult<Stmt> {
        // This will delegate to the stmt parser module
        // For now, return a placeholder
        todo!("Statement parsing will be implemented in stmt.rs")
    }

    // Token stream management methods

    /// Check if we're at the end of the token stream
    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || matches!(self.peek().token_type, TokenType::Eof)
    }

    /// Get the current token without advancing
    pub fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or(&EOF_TOKEN)
    }

    /// Get the token at the given offset from current position
    pub fn peek_ahead(&self, offset: usize) -> &Token {
        let index = self.current + offset;
        self.tokens.get(index).unwrap_or(&EOF_TOKEN)
    }

    /// Get the previous token
    pub fn previous(&self) -> Option<&Token> {
        self.previous.as_ref()
    }

    /// Advance to the next token and return the current one
    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.previous = Some(self.peek().clone());
            self.current += 1;
        }
        self.previous().unwrap_or_else(|| self.peek()) // Fallback to current if no previous
    }

    /// Check if the current token matches the given type
    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(token_type)
        }
    }

    /// Check if the current token matches any of the given types
    pub fn check_any(&self, token_types: &[TokenType]) -> bool {
        token_types.iter().any(|t| self.check(t))
    }

    /// Consume a token if it matches the expected type
    pub fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Consume a token if it matches any of the expected types
    pub fn match_any(&mut self, token_types: &[TokenType]) -> Option<TokenType> {
        for token_type in token_types {
            if self.check(token_type) {
                let matched = self.peek().token_type.clone();
                self.advance();
                return Some(matched);
            }
        }
        None
    }

    /// Consume and return a token, expecting it to match the given type
    pub fn consume(&mut self, token_type: &TokenType, message: &str) -> ParseResult<Token> {
        if self.check(token_type) {
            Ok(self.advance().clone())
        } else {
            let current = self.peek();
            Err(ParseError::expected_token(message, current))
        }
    }

    /// Skip trivia tokens (whitespace, comments, newlines based on config)
    pub fn skip_trivia(&mut self) {
        while !self.is_at_end() {
            match &self.peek().token_type {
                TokenType::LineComment(_) | TokenType::BlockComment(_)
                    if self.config.skip_comments =>
                {
                    self.advance();
                }
                TokenType::Newline if self.config.skip_newlines => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    // Error handling and recovery methods

    /// Record an error for later reporting
    fn record_error(&mut self, error: ParseError) {
        self.error_recovery.errors.push(error);
        self.error_recovery.in_recovery = true;
    }

    /// Attempt to recover to a synchronization point
    fn recover_to_sync_point(&mut self) -> bool {
        while !self.is_at_end() {
            // Check if current token is a sync point
            if self
                .error_recovery
                .sync_tokens
                .iter()
                .any(|t| self.check(t))
            {
                self.error_recovery.in_recovery = false;
                return true;
            }

            // Special recovery for block structures
            if matches!(self.peek().token_type, TokenType::End) {
                self.error_recovery.in_recovery = false;
                return true;
            }

            self.advance();
        }

        false // Couldn't find a sync point
    }

    /// Check if we're currently in error recovery mode
    pub fn in_error_recovery(&self) -> bool {
        self.error_recovery.in_recovery
    }

    /// Get all collected errors
    pub fn errors(&self) -> &[ParseError] {
        &self.error_recovery.errors
    }

    /// Clear all collected errors
    pub fn clear_errors(&mut self) {
        self.error_recovery.errors.clear();
        self.error_recovery.in_recovery = false;
    }

    // Utility methods for common parsing patterns

    /// Parse a comma-separated list of items
    pub fn parse_comma_separated<T, F>(&mut self, mut parse_item: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(&mut Self) -> ParseResult<T>,
    {
        let mut items = Vec::new();

        // Handle empty list
        if self.check(&TokenType::RightParen)
            || self.check(&TokenType::RightBracket)
            || self.check(&TokenType::RightBrace)
        {
            return Ok(items);
        }

        loop {
            items.push(parse_item(self)?);

            if !self.match_token(&TokenType::Comma) {
                break;
            }

            // Allow trailing comma
            if self.check(&TokenType::RightParen)
                || self.check(&TokenType::RightBracket)
                || self.check(&TokenType::RightBrace)
            {
                break;
            }
        }

        Ok(items)
    }

    /// Parse a block structure (: ... end)
    pub fn parse_block<T, F>(&mut self, mut parse_content: F) -> ParseResult<T>
    where
        F: FnMut(&mut Self) -> ParseResult<T>,
    {
        self.consume(&TokenType::Colon, "expected ':' to start block")?;
        self.skip_trivia();

        let content = parse_content(self)?;

        self.skip_trivia();
        self.consume(&TokenType::End, "expected 'end' to close block")?;

        Ok(content)
    }

    /// Get the current parsing position for error reporting
    pub fn current_position(&self) -> Position {
        self.peek().span.start
    }

    /// Get the current span for error reporting
    pub fn current_span(&self) -> Span {
        self.peek().span
    }

    /// Create a span from start position to current position
    pub fn span_from(&self, start: Position) -> Span {
        Span::new(start, self.current_position())
    }
}

// Integration with nom parser combinators
impl Parser {
    /// Convert the current token slice to a nom-compatible input
    pub fn as_nom_input(&self) -> &[Token] {
        &self.tokens[self.current..]
    }

    /// Update parser state from nom parsing result
    pub fn update_from_nom(&mut self, remaining: &[Token]) {
        let consumed = self.tokens.len() - self.current - remaining.len();
        self.current += consumed;
        if consumed > 0 && self.current > 0 {
            self.previous = Some(self.tokens[self.current - 1].clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bolt_lexer::{Lexer, TokenType};

    fn create_test_parser(source: &str) -> Parser {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize test input");
        Parser::new(tokens)
    }

    #[test]
    fn test_parser_creation() {
        let parser = create_test_parser("let x = 42");
        assert!(!parser.is_at_end());
        assert!(matches!(parser.peek().token_type, TokenType::Let));
    }

    #[test]
    fn test_token_navigation() {
        let mut parser = create_test_parser("let x = 42");

        // Test peek
        assert!(matches!(parser.peek().token_type, TokenType::Let));

        // Test advance
        let token = parser.advance();
        assert!(matches!(token.token_type, TokenType::Let));
        assert!(matches!(parser.peek().token_type, TokenType::Identifier(_)));

        // Test check
        assert!(parser.check(&TokenType::Identifier("x".to_string())));
        assert!(!parser.check(&TokenType::Let));

        // Test peek_ahead
        assert!(matches!(parser.peek_ahead(1).token_type, TokenType::Equal)); // =
    }

    #[test]
    fn test_match_token() {
        let mut parser = create_test_parser("let mut x");

        assert!(parser.match_token(&TokenType::Let));
        assert!(parser.match_token(&TokenType::Mut));
        assert!(!parser.match_token(&TokenType::Fn)); // Should not advance
        assert!(matches!(parser.peek().token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_consume_success() {
        let mut parser = create_test_parser("let x");

        let token = parser.consume(&TokenType::Let, "expected 'let'").unwrap();
        assert!(matches!(token.token_type, TokenType::Let));
    }

    #[test]
    fn test_consume_failure() {
        let mut parser = create_test_parser("let x");

        let result = parser.consume(&TokenType::Fn, "expected 'fn'");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ParseError::ExpectedToken { .. }
        ));
    }

    #[test]
    fn test_skip_trivia() {
        let mut parser = create_test_parser("// comment\n\nlet x");

        parser.skip_trivia();
        assert!(matches!(parser.peek().token_type, TokenType::Let));
    }

    #[test]
    fn test_comma_separated_parsing() {
        let mut parser = create_test_parser("1, 2, 3");

        let items = parser
            .parse_comma_separated(|p| {
                if let TokenType::Integer(n) = &p.peek().token_type {
                    let value = *n;
                    p.advance();
                    Ok(value)
                } else {
                    Err(ParseError::expected_token("integer", p.peek()))
                }
            })
            .unwrap();

        assert_eq!(items, vec![1, 2, 3]);
    }

    #[test]
    fn test_block_parsing() {
        let mut parser = create_test_parser(": 42 end");

        let result = parser
            .parse_block(|p| {
                if let TokenType::Integer(n) = &p.peek().token_type {
                    let value = *n;
                    p.advance();
                    Ok(value)
                } else {
                    Err(ParseError::expected_token("integer", p.peek()))
                }
            })
            .unwrap();

        assert_eq!(result, 42);
    }

    #[test]
    fn test_error_recovery() {
        let mut parser = create_test_parser("invalid let x = 42");
        parser.config.enable_recovery = true;

        // Force an error by trying to consume 'let' when we're at 'invalid'
        let error = parser
            .consume(&TokenType::Let, "expected 'let'")
            .unwrap_err();
        parser.record_error(error);

        // Recovery should find the 'let' token
        assert!(parser.recover_to_sync_point());
        assert!(matches!(parser.peek().token_type, TokenType::Let));
    }

    #[test]
    fn test_parser_config() {
        let config = ParserConfig {
            allow_incomplete: true,
            skip_comments: false,
            ..Default::default()
        };

        let parser = Parser::with_config(vec![], config.clone());
        assert_eq!(parser.config.allow_incomplete, true);
        assert_eq!(parser.config.skip_comments, false);
    }

    #[test]
    fn test_position_tracking() {
        let mut parser = create_test_parser("let\nx");

        let start_pos = parser.current_position();
        parser.advance(); // Consume 'let'
        parser.advance(); // Consume newline

        let span = parser.span_from(start_pos);
        assert_eq!(span.start.line, 1);
        assert!(span.end.line >= span.start.line);
    }
}
