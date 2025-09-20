use crate::position::{Position, Span};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Token types in the Bolt language
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TokenType {
    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),

    // Identifiers
    Identifier(String),

    // Keywords
    Let,
    Mut,
    Fn,
    Pure,
    Class,
    Struct,
    Enum,
    Trait,
    Impl,
    Module,
    Import,
    Async,
    Await,
    Spawn,
    Join,
    Defer,
    Match,
    If,
    Else,
    Loop,
    Return,
    Override,
    Super,
    Extern,
    Lazy,
    Comptime,
    Pub,
    Protected,
    Internal,
    End,

    // Operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Caret,      // ^
    Percent,    // %
    PlusPlus,   // ++
    MinusMinus, // --
    PlusEqual,  // +=
    MinusEqual, // -=
    StarEqual,  // *=
    SlashEqual, // /=
    TildeEqual, // ~= (concat)

    // Comparison (methods)
    Dot,      // .
    Question, // ?

    // Boolean operators
    And, // and
    Or,  // or
    Xor, // xor
    Not, // not

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }

    // Punctuation
    Colon,     // :
    Semicolon, // ;
    Comma,     // ,
    Arrow,     // =>

    // Special symbols
    Dollar,       // $
    DoubleColon,  // ::
    Ampersand,    // &
    AmpersandMut, // &mut

    // Comments
    LineComment(String),
    BlockComment(String),

    // Special tokens
    Newline,
    Eof,

    // String interpolation
    InterpolationStart, // {
    InterpolationEnd,   // }
    InterpolatedText(String),
}

// Custom PartialEq implementation to handle f64 comparison
impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenType::Integer(a), TokenType::Integer(b)) => a == b,
            (TokenType::Float(a), TokenType::Float(b)) => {
                // Handle NaN and infinity cases properly
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            (TokenType::String(a), TokenType::String(b)) => a == b,
            (TokenType::Boolean(a), TokenType::Boolean(b)) => a == b,
            (TokenType::Identifier(a), TokenType::Identifier(b)) => a == b,
            (TokenType::LineComment(a), TokenType::LineComment(b)) => a == b,
            (TokenType::BlockComment(a), TokenType::BlockComment(b)) => a == b,
            (TokenType::InterpolatedText(a), TokenType::InterpolatedText(b)) => a == b,
            // For all other variants, use discriminant comparison
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

// Implement Eq since we've properly handled the float case
impl Eq for TokenType {}

/// A token with its type, span, and source text
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
    pub text: String,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span, text: String) -> Self {
        Self {
            token_type,
            span,
            text,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Integer(n) => write!(f, "{}", n),
            TokenType::Float(n) => write!(f, "{}", n),
            TokenType::String(s) => write!(f, "\"{}\"", s),
            TokenType::Boolean(b) => write!(f, "{}", b),
            TokenType::Identifier(name) => write!(f, "{}", name),
            TokenType::Let => write!(f, "let"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Pure => write!(f, "pure"),
            TokenType::Class => write!(f, "class"),
            TokenType::Struct => write!(f, "struct"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::Trait => write!(f, "trait"),
            TokenType::Impl => write!(f, "impl"),
            TokenType::Module => write!(f, "module"),
            TokenType::Import => write!(f, "import"),
            TokenType::Async => write!(f, "async"),
            TokenType::Await => write!(f, "await"),
            TokenType::Spawn => write!(f, "spawn"),
            TokenType::Join => write!(f, "join"),
            TokenType::Defer => write!(f, "defer"),
            TokenType::Match => write!(f, "match"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::Loop => write!(f, "loop"),
            TokenType::Return => write!(f, "return"),
            TokenType::Override => write!(f, "override"),
            TokenType::Super => write!(f, "super"),
            TokenType::Extern => write!(f, "extern"),
            TokenType::Lazy => write!(f, "lazy"),
            TokenType::Comptime => write!(f, "comptime"),
            TokenType::Pub => write!(f, "pub"),
            TokenType::Protected => write!(f, "protected"),
            TokenType::Internal => write!(f, "internal"),
            TokenType::End => write!(f, "end"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Caret => write!(f, "^"),
            TokenType::Percent => write!(f, "%"),
            TokenType::PlusPlus => write!(f, "++"),
            TokenType::MinusMinus => write!(f, "--"),
            TokenType::PlusEqual => write!(f, "+="),
            TokenType::MinusEqual => write!(f, "-="),
            TokenType::StarEqual => write!(f, "*="),
            TokenType::SlashEqual => write!(f, "/="),
            TokenType::TildeEqual => write!(f, "~="),
            TokenType::Dot => write!(f, "."),
            TokenType::Question => write!(f, "?"),
            TokenType::And => write!(f, "and"),
            TokenType::Or => write!(f, "or"),
            TokenType::Xor => write!(f, "xor"),
            TokenType::Not => write!(f, "not"),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Comma => write!(f, ","),
            TokenType::Arrow => write!(f, "=>"),
            TokenType::Dollar => write!(f, "$"),
            TokenType::DoubleColon => write!(f, "::"),
            TokenType::Ampersand => write!(f, "&"),
            TokenType::AmpersandMut => write!(f, "&mut"),
            TokenType::LineComment(comment) => write!(f, "//{}", comment),
            TokenType::BlockComment(comment) => write!(f, "/*{}*/", comment),
            TokenType::Newline => write!(f, "\\n"),
            TokenType::Eof => write!(f, "EOF"),
            TokenType::InterpolationStart => write!(f, "{{"),
            TokenType::InterpolationEnd => write!(f, "}}"),
            TokenType::InterpolatedText(text) => write!(f, "{}", text),
        }
    }
}

/// Helper function to check if a string is a keyword
pub fn keyword_from_str(s: &str) -> Option<TokenType> {
    match s {
        "let" => Some(TokenType::Let),
        "mut" => Some(TokenType::Mut),
        "fn" => Some(TokenType::Fn),
        "pure" => Some(TokenType::Pure),
        "class" => Some(TokenType::Class),
        "struct" => Some(TokenType::Struct),
        "enum" => Some(TokenType::Enum),
        "trait" => Some(TokenType::Trait),
        "impl" => Some(TokenType::Impl),
        "module" => Some(TokenType::Module),
        "import" => Some(TokenType::Import),
        "async" => Some(TokenType::Async),
        "await" => Some(TokenType::Await),
        "spawn" => Some(TokenType::Spawn),
        "join" => Some(TokenType::Join),
        "defer" => Some(TokenType::Defer),
        "match" => Some(TokenType::Match),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "loop" => Some(TokenType::Loop),
        "return" => Some(TokenType::Return),
        "override" => Some(TokenType::Override),
        "super" => Some(TokenType::Super),
        "extern" => Some(TokenType::Extern),
        "lazy" => Some(TokenType::Lazy),
        "comptime" => Some(TokenType::Comptime),
        "pub" => Some(TokenType::Pub),
        "protected" => Some(TokenType::Protected),
        "internal" => Some(TokenType::Internal),
        "end" => Some(TokenType::End),
        "and" => Some(TokenType::And),
        "or" => Some(TokenType::Or),
        "xor" => Some(TokenType::Xor),
        "not" => Some(TokenType::Not),
        "true" => Some(TokenType::Boolean(true)),
        "false" => Some(TokenType::Boolean(false)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_recognition() {
        assert_eq!(keyword_from_str("let"), Some(TokenType::Let));
        assert_eq!(keyword_from_str("fn"), Some(TokenType::Fn));
        assert_eq!(keyword_from_str("true"), Some(TokenType::Boolean(true)));
        assert_eq!(keyword_from_str("false"), Some(TokenType::Boolean(false)));
        assert_eq!(keyword_from_str("not_a_keyword"), None);
    }

    #[test]
    fn test_token_display() {
        let token_type = TokenType::Let;
        assert_eq!(format!("{}", token_type), "let");

        let token_type = TokenType::Integer(42);
        assert_eq!(format!("{}", token_type), "42");

        let token_type = TokenType::String("hello".to_string());
        assert_eq!(format!("{}", token_type), "\"hello\"");
    }

    #[test]
    fn test_float_equality() {
        let float1 = TokenType::Float(3.14);
        let float2 = TokenType::Float(3.14);
        let float_nan1 = TokenType::Float(f64::NAN);
        let float_nan2 = TokenType::Float(f64::NAN);

        assert_eq!(float1, float2);
        assert_eq!(float_nan1, float_nan2); // NaN should equal NaN for our tokens
    }

    #[test]
    fn test_token_creation() {
        let pos = Position::new(1, 1, 0);
        let span = Span::single(pos);
        let token = Token::new(TokenType::Let, span, "let".to_string());

        assert_eq!(token.token_type, TokenType::Let);
        assert_eq!(token.span, span);
        assert_eq!(token.text, "let");
    }
}
