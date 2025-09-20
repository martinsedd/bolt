use crate::error::LexError;
use crate::position::{Position, Span};
use crate::token::{Token, TokenType, keyword_from_str};

/// The main lexer struct
pub struct Lexer {
    input: Vec<char>,
    position: Position,
    current: usize, // Current character index in input
    start: usize,   // Start of current token
}

impl Lexer {
    /// Create a new lexer from source code
    pub fn new(source: &str) -> Self {
        Self {
            input: source.chars().collect(),
            position: Position::default(),
            current: 0,
            start: 0,
        }
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    /// Get the next token from the input
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        self.start = self.current;
        let start_pos = self.position;

        if self.is_at_end() {
            return Ok(Token::new(
                TokenType::Eof,
                Span::single(self.position),
                String::new(),
            ));
        }

        let ch = self.advance();

        match ch {
            // Newlines (significant in some contexts)
            '\n' => Ok(self.make_token(TokenType::Newline, start_pos)),

            // Single-character tokens
            '(' => Ok(self.make_token(TokenType::LeftParen, start_pos)),
            ')' => Ok(self.make_token(TokenType::RightParen, start_pos)),
            '[' => Ok(self.make_token(TokenType::LeftBracket, start_pos)),
            ']' => Ok(self.make_token(TokenType::RightBracket, start_pos)),
            '{' => Ok(self.make_token(TokenType::LeftBrace, start_pos)),
            '}' => Ok(self.make_token(TokenType::RightBrace, start_pos)),
            ';' => Ok(self.make_token(TokenType::Semicolon, start_pos)),
            ',' => Ok(self.make_token(TokenType::Comma, start_pos)),
            '.' => Ok(self.make_token(TokenType::Dot, start_pos)),
            '?' => Ok(self.make_token(TokenType::Question, start_pos)),
            '$' => Ok(self.make_token(TokenType::Dollar, start_pos)),
            '%' => Ok(self.make_token(TokenType::Percent, start_pos)),
            '^' => Ok(self.make_token(TokenType::Caret, start_pos)),

            // Potentially multi-character tokens
            '+' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::PlusEqual, start_pos))
                } else if self.match_char('+') {
                    Ok(self.make_token(TokenType::PlusPlus, start_pos))
                } else {
                    Ok(self.make_token(TokenType::Plus, start_pos))
                }
            }
            '-' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::MinusEqual, start_pos))
                } else if self.match_char('-') {
                    Ok(self.make_token(TokenType::MinusMinus, start_pos))
                } else {
                    Ok(self.make_token(TokenType::Minus, start_pos))
                }
            }
            '*' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::StarEqual, start_pos))
                } else {
                    Ok(self.make_token(TokenType::Star, start_pos))
                }
            }
            '/' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::SlashEqual, start_pos))
                } else if self.match_char('/') {
                    self.line_comment()
                } else if self.match_char('*') {
                    self.block_comment(start_pos)
                } else {
                    Ok(self.make_token(TokenType::Slash, start_pos))
                }
            }
            '~' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenType::TildeEqual, start_pos))
                } else {
                    Err(LexError::UnexpectedCharacter(
                        ch,
                        start_pos.line,
                        start_pos.column,
                    ))
                }
            }
            '=' => {
                if self.match_char('>') {
                    Ok(self.make_token(TokenType::Arrow, start_pos))
                } else {
                    Err(LexError::UnexpectedCharacter(
                        ch,
                        start_pos.line,
                        start_pos.column,
                    ))
                }
            }
            ':' => {
                if self.match_char(':') {
                    Ok(self.make_token(TokenType::DoubleColon, start_pos))
                } else {
                    Ok(self.make_token(TokenType::Colon, start_pos))
                }
            }
            '&' => {
                // Look ahead for "mut"
                if self.current + 2 < self.input.len()
                    && self.input[self.current] == 'm'
                    && self.input[self.current + 1] == 'u'
                    && self.input[self.current + 2] == 't'
                    && (self.current + 3 >= self.input.len()
                        || !self.input[self.current + 3].is_alphanumeric())
                {
                    // Consume "mut"
                    self.advance(); // m
                    self.advance(); // u
                    self.advance(); // t
                    Ok(self.make_token(TokenType::AmpersandMut, start_pos))
                } else {
                    Ok(self.make_token(TokenType::Ampersand, start_pos))
                }
            }

            // String literals
            '"' => self.string_literal(start_pos),

            // Numbers
            c if c.is_ascii_digit() => self.number(start_pos),

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' => self.identifier(start_pos),

            // Unexpected character
            _ => Err(LexError::UnexpectedCharacter(
                ch,
                start_pos.line,
                start_pos.column,
            )),
        }
    }

    // Helper methods

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            let ch = self.input[self.current];
            self.current += 1;
            self.position.advance_char(ch);
            ch
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.input.len() {
            '\0'
        } else {
            self.input[self.current + 1]
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.input[self.current] != expected {
            false
        } else {
            self.current += 1;
            self.position.advance_char(expected);
            true
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn make_token(&self, token_type: TokenType, start_pos: Position) -> Token {
        let text: String = self.input[self.start..self.current].iter().collect();
        Token::new(token_type, Span::new(start_pos, self.position), text)
    }

    fn line_comment(&mut self) -> Result<Token, LexError> {
        let start_pos = Position::new(
            self.position.line,
            self.position.column - 2, // Account for "//"
            self.position.offset - 2,
        );

        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }

        let comment_text: String = self.input[self.start + 2..self.current].iter().collect();
        let full_text: String = self.input[self.start..self.current].iter().collect();

        Ok(Token::new(
            TokenType::LineComment(comment_text),
            Span::new(start_pos, self.position),
            full_text,
        ))
    }

    fn block_comment(&mut self, start_pos: Position) -> Result<Token, LexError> {
        let mut depth = 1;

        while !self.is_at_end() && depth > 0 {
            let ch = self.peek();

            if ch == '/' && self.peek_next() == '*' {
                self.advance(); // /
                self.advance(); // *
                depth += 1;
            } else if ch == '*' && self.peek_next() == '/' {
                self.advance(); // *
                self.advance(); // /
                depth -= 1;
            } else {
                self.advance();
            }
        }

        if depth > 0 {
            Err(LexError::UnterminatedBlockComment(
                start_pos.line,
                start_pos.column,
            ))
        } else {
            let comment_text: String = self.input[self.start + 2..self.current - 2]
                .iter()
                .collect();
            let full_text: String = self.input[self.start..self.current].iter().collect();

            Ok(Token::new(
                TokenType::BlockComment(comment_text),
                Span::new(start_pos, self.position),
                full_text,
            ))
        }
    }

    fn string_literal(&mut self, start_pos: Position) -> Result<Token, LexError> {
        let mut value = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            let ch = self.advance();

            if ch == '\\' {
                // Handle escape sequences
                if self.is_at_end() {
                    return Err(LexError::UnterminatedString(
                        start_pos.line,
                        start_pos.column,
                    ));
                }

                let escaped = self.advance();
                match escaped {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    '\'' => value.push('\''),
                    '0' => value.push('\0'),
                    _ => {
                        return Err(LexError::InvalidEscapeSequence(
                            escaped,
                            self.position.line,
                            self.position.column - 1,
                        ));
                    }
                }
            } else {
                value.push(ch);
            }
        }

        if self.is_at_end() {
            Err(LexError::UnterminatedString(
                start_pos.line,
                start_pos.column,
            ))
        } else {
            // Consume closing quote
            self.advance();

            let full_text: String = self.input[self.start..self.current].iter().collect();

            Ok(Token::new(
                TokenType::String(value),
                Span::new(start_pos, self.position),
                full_text,
            ))
        }
    }

    fn number(&mut self, start_pos: Position) -> Result<Token, LexError> {
        // Consume all digits
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for decimal point
        if !self.is_at_end() && self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the '.'
            self.advance();

            // Consume fractional part
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.advance();
            }

            let text: String = self.input[self.start..self.current].iter().collect();

            match text.parse::<f64>() {
                Ok(value) => Ok(Token::new(
                    TokenType::Float(value),
                    Span::new(start_pos, self.position),
                    text,
                )),
                Err(_) => Err(LexError::InvalidNumber(
                    text,
                    start_pos.line,
                    start_pos.column,
                )),
            }
        } else {
            let text: String = self.input[self.start..self.current].iter().collect();

            match text.parse::<i64>() {
                Ok(value) => Ok(Token::new(
                    TokenType::Integer(value),
                    Span::new(start_pos, self.position),
                    text,
                )),
                Err(_) => Err(LexError::InvalidNumber(
                    text,
                    start_pos.line,
                    start_pos.column,
                )),
            }
        }
    }

    fn identifier(&mut self, start_pos: Position) -> Result<Token, LexError> {
        while !self.is_at_end() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let text: String = self.input[self.start..self.current].iter().collect();
        let token_type = keyword_from_str(&text).unwrap_or(TokenType::Identifier(text.clone()));

        Ok(Token::new(
            token_type,
            Span::new(start_pos, self.position),
            text,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token_type, TokenType::Eof));
    }

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("( ) [ ] { }");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 7); // 6 tokens + EOF
        assert!(matches!(tokens[0].token_type, TokenType::LeftParen));
        assert!(matches!(tokens[1].token_type, TokenType::RightParen));
        assert!(matches!(tokens[2].token_type, TokenType::LeftBracket));
        assert!(matches!(tokens[3].token_type, TokenType::RightBracket));
        assert!(matches!(tokens[4].token_type, TokenType::LeftBrace));
        assert!(matches!(tokens[5].token_type, TokenType::RightBrace));
        assert!(matches!(tokens[6].token_type, TokenType::Eof));
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("let mut fn class");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 5); // 4 keywords + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Let));
        assert!(matches!(tokens[1].token_type, TokenType::Mut));
        assert!(matches!(tokens[2].token_type, TokenType::Fn));
        assert!(matches!(tokens[3].token_type, TokenType::Class));
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("hello world _private");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // 3 identifiers + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Identifier(ref s) if s == "hello"));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(ref s) if s == "world"));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(ref s) if s == "_private"));
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("42 3.14 0");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // 3 numbers + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Integer(42)));
        assert!(
            matches!(tokens[1].token_type, TokenType::Float(f) if (f - 3.14).abs() < f64::EPSILON)
        );
        assert!(matches!(tokens[2].token_type, TokenType::Integer(0)));
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new(r#""hello" "world\n""#);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 3); // 2 strings + EOF
        assert!(matches!(tokens[0].token_type, TokenType::String(ref s) if s == "hello"));
        assert!(matches!(tokens[1].token_type, TokenType::String(ref s) if s == "world\n"));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ ++ += - -- -= * *= / /= ~= => :: &mut");
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].token_type, TokenType::Plus));
        assert!(matches!(tokens[1].token_type, TokenType::PlusPlus));
        assert!(matches!(tokens[2].token_type, TokenType::PlusEqual));
        assert!(matches!(tokens[3].token_type, TokenType::Minus));
        assert!(matches!(tokens[4].token_type, TokenType::MinusMinus));
        assert!(matches!(tokens[5].token_type, TokenType::MinusEqual));
        assert!(matches!(tokens[6].token_type, TokenType::Star));
        assert!(matches!(tokens[7].token_type, TokenType::StarEqual));
        assert!(matches!(tokens[8].token_type, TokenType::Slash));
        assert!(matches!(tokens[9].token_type, TokenType::SlashEqual));
        assert!(matches!(tokens[10].token_type, TokenType::TildeEqual));
        assert!(matches!(tokens[11].token_type, TokenType::Arrow));
        assert!(matches!(tokens[12].token_type, TokenType::DoubleColon));
        assert!(matches!(tokens[13].token_type, TokenType::AmpersandMut));
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("// line comment\n/* block comment */");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // line comment + newline + block comment + EOF
        assert!(
            matches!(tokens[0].token_type, TokenType::LineComment(ref s) if s == " line comment")
        );
        assert!(matches!(tokens[1].token_type, TokenType::Newline));
        assert!(
            matches!(tokens[2].token_type, TokenType::BlockComment(ref s) if s == " block comment ")
        );
    }

    #[test]
    fn test_error_handling() {
        let mut lexer = Lexer::new("@");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            LexError::UnexpectedCharacter('@', 1, 1)
        ));
    }

    #[test]
    fn test_unterminated_string() {
        let mut lexer = Lexer::new("\"unterminated");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            LexError::UnterminatedString(1, 1)
        ));
    }

    #[test]
    fn test_bolt_hello_world() {
        let source = r#"
fn main() :
  print("Hello, Bolt!")
end
"#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        // Should tokenize without errors
        assert!(!tokens.is_empty());
        assert!(matches!(tokens.last().unwrap().token_type, TokenType::Eof));
    }
}
