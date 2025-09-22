use crate::error::{ParseError, ParseResult};
use crate::parser::Parser;
use bolt_ast::{GenericParam, TraitBound, Type, TypeAnnotation};
use bolt_lexer::{Span, TokenType};

impl Parser {
    /// Parse a type annotation (:: Type)
    pub fn parse_type_annotation(&mut self) -> ParseResult<TypeAnnotation> {
        let start_pos = self.current_position();

        if self.match_token(&TokenType::DoubleColon) {
            // Explicit type annotation: :: Type
            let explicit_type = self.parse_type()?;
            let span = self.span_from(start_pos);
            Ok(TypeAnnotation::explicit(explicit_type, span))
        } else {
            // No explicit type, will be inferred
            let span = Span::single(start_pos);
            Ok(TypeAnnotation::inferred(span))
        }
    }

    pub fn parse_type(&mut self) -> ParseResult<Type> {
        self.parse_function_type()
    }

    /// Parse function types: (T1, T2) -> T3 or T1 -> T2
    fn parse_function_type(&mut self) -> ParseResult<Type> {
        let mut base_type = self.parse_reference_type()?;

        // Check for function arrow
        if self.match_token(&TokenType::FunctionArrow) {
            // Simple function: T1 -> T2
            let return_type = self.parse_type()?;
            base_type = Type::Function {
                params: vec![base_type],
                return_type: Box::new(return_type),
            };
        }

        Ok(base_type)
    }

    /// Parse reference types: &T, &mut T
    fn parse_reference_type(&mut self) -> ParseResult<Type> {
        if self.match_token(&TokenType::Ampersand) {
            let mutable = self.match_token(&TokenType::Mut);
            let inner_type = self.parse_reference_type()?; // Allow nested references
            Ok(Type::Reference {
                mutable,
                inner: Box::new(inner_type),
            })
        } else if self.match_token(&TokenType::AmpersandMut) {
            let inner_type = self.parse_reference_type()?;
            Ok(Type::Reference {
                mutable: true,
                inner: Box::new(inner_type),
            })
        } else {
            self.parse_primary_type()
        }
    }

    /// Parse primary types: Int, [T], {K: V}, User<T>, etc.
    fn parse_primary_type(&mut self) -> ParseResult<Type> {
        match &self.peek().token_type {
            // Primitive types
            TokenType::Identifier(name) => {
                let type_name = name.clone();
                self.advance();

                match type_name.as_str() {
                    "Int" => Ok(Type::Int),
                    "Float" => Ok(Type::Float),
                    "Bool" => Ok(Type::Bool),
                    "String" => Ok(Type::String),
                    "Result" => {
                        // Result<T, E>
                        if self.check(&TokenType::Less) {
                            let generics = self.parse_generic_args()?;
                            if generics.len() != 2 {
                                return Err(ParseError::invalid_type(
                                    "Result type requires exactly 2 type parameters",
                                    self.current_span(),
                                ));
                            }
                            Ok(Type::Result(
                                Box::new(generics[0].clone()),
                                Box::new(generics[1].clone()),
                            ))
                        } else {
                            return Err(ParseError::invalid_type(
                                "Result type requires type parameters: Result<T, E>",
                                self.current_span(),
                            ));
                        }
                    }
                    "Maybe" => {
                        // Maybe<T>
                        if self.check(&TokenType::Less) {
                            let generics = self.parse_generic_args()?;
                            if generics.len() != 1 {
                                return Err(ParseError::invalid_type(
                                    "Maybe type requires exactly 1 type parameter",
                                    self.current_span(),
                                ));
                            }
                            Ok(Type::Maybe(Box::new(generics[0].clone())))
                        } else {
                            return Err(ParseError::invalid_type(
                                "Maybe type requires type parameter: Maybe<T>",
                                self.current_span(),
                            ));
                        }
                    }
                    _ => {
                        // User-defined type
                        let generics = if self.check(&TokenType::Less) {
                            self.parse_generic_args()?
                        } else {
                            vec![]
                        };
                        Ok(Type::UserDefined {
                            name: type_name,
                            generics,
                        })
                    }
                }
            }

            // Array type: [T]
            TokenType::LeftBracket => {
                self.advance(); // consume '['
                let element_type = self.parse_type()?;
                self.consume(
                    &TokenType::RightBracket,
                    "expected ']' after array element type",
                )?;
                Ok(Type::Array(Box::new(element_type)))
            }

            // Map type: {K: V} or tuple type if parentheses
            TokenType::LeftBrace => {
                self.advance(); // consume '{'

                // Check for empty map type
                if self.match_token(&TokenType::Colon) {
                    self.consume(&TokenType::RightBrace, "expected '}' after empty map type")?;
                    return Ok(Type::Map(Box::new(Type::Unknown), Box::new(Type::Unknown)));
                }

                let key_type = self.parse_type()?;
                self.consume(&TokenType::Colon, "expected ':' in map type")?;
                let value_type = self.parse_type()?;
                self.consume(&TokenType::RightBrace, "expected '}' after map type")?;
                Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
            }

            // Parenthesized type or function type: (T) or (T1, T2) -> T3
            TokenType::LeftParen => {
                self.advance(); // consume '('

                // Check for unit type: ()
                if self.check(&TokenType::RightParen) {
                    self.advance(); // consume ')'

                    // Check if this is a function type: () -> T
                    if self.match_token(&TokenType::FunctionArrow) {
                        let return_type = self.parse_type()?;
                        Ok(Type::Function {
                            params: vec![],
                            return_type: Box::new(return_type),
                        })
                    } else {
                        Ok(Type::Unit)
                    }
                } else {
                    // Parse comma-separated parameter types
                    let param_types = self.parse_comma_separated(|parser| parser.parse_type())?;
                    self.consume(&TokenType::RightParen, "expected ')' after parameter types")?;

                    // Check if this is a function type
                    if self.match_token(&TokenType::FunctionArrow) {
                        let return_type = self.parse_type()?;
                        Ok(Type::Function {
                            params: param_types,
                            return_type: Box::new(return_type),
                        })
                    } else if param_types.len() == 1 {
                        // Single parenthesized type
                        Ok(param_types.into_iter().next().unwrap())
                    } else {
                        return Err(ParseError::invalid_type(
                            "Multiple types in parentheses must be function parameters",
                            self.current_span(),
                        ));
                    }
                }
            }

            // Question mark for unknown/inferred type
            TokenType::Question => {
                self.advance();
                Ok(Type::Unknown)
            }

            _ => Err(ParseError::expected_token("type", self.peek())),
        }
    }

    /// Parse generic type arguments: <T1, T2, T3>
    fn parse_generic_args(&mut self) -> ParseResult<Vec<Type>> {
        self.consume(&TokenType::Less, "expected '<' for generic arguments")?;

        let args = self.parse_comma_separated(|parser| parser.parse_type())?;

        self.consume(&TokenType::Greater, "expected '>' after generic arguments")?;
        Ok(args)
    }

    /// Parse generic parameter definitions: <T: Bound1 + Bound2, U>
    pub fn parse_generic_params(&mut self) -> ParseResult<Vec<GenericParam>> {
        if !self.check(&TokenType::Less) {
            return Ok(vec![]); // No generics
        }

        self.advance(); // consume '<'

        let params = self.parse_comma_separated(|parser| parser.parse_generic_param())?;

        self.consume(&TokenType::Greater, "expected '>' after generic parameters")?;
        Ok(params)
    }

    /// Parse a single generic parameter: T: Bound1 + Bound2
    fn parse_generic_param(&mut self) -> ParseResult<GenericParam> {
        let start_pos = self.current_position();

        // Parse parameter name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token(
                "generic parameter name",
                self.peek(),
            ));
        };

        // Parse optional bounds: : Trait1 + Trait2
        let bounds = if self.match_token(&TokenType::Colon) {
            self.parse_trait_bounds()?
        } else {
            vec![]
        };

        let span = self.span_from(start_pos);
        Ok(GenericParam { name, bounds, span })
    }

    /// Parse trait bounds: Trait1 + Trait2 + Trait3
    fn parse_trait_bounds(&mut self) -> ParseResult<Vec<TraitBound>> {
        let mut bounds = vec![];

        loop {
            let start_pos = self.current_position();

            // Parse trait name
            let name = if let TokenType::Identifier(name) = &self.peek().token_type {
                let name = name.clone();
                self.advance();
                name
            } else {
                return Err(ParseError::expected_token("trait name", self.peek()));
            };

            let span = self.span_from(start_pos);
            bounds.push(TraitBound { name, span });

            // Check for more bounds
            if !self.match_token(&TokenType::Plus) {
                break;
            }
        }

        Ok(bounds)
    }

    /// Parse a visibility modifier: pub, protected, etc.
    pub fn parse_visibility(&mut self) -> ParseResult<bolt_ast::Visibility> {
        match &self.peek().token_type {
            TokenType::Pub => {
                self.advance();

                // Check for pub(modifier)
                if self.match_token(&TokenType::LeftParen) {
                    let modifier = if let TokenType::Identifier(name) = &self.peek().token_type {
                        let name = name.clone();
                        self.advance();
                        name
                    } else {
                        return Err(ParseError::invalid_visibility(
                            "expected visibility modifier after 'pub('",
                            self.current_span(),
                        ));
                    };

                    self.consume(
                        &TokenType::RightParen,
                        "expected ')' after visibility modifier",
                    )?;

                    match modifier.as_str() {
                        "package" => Ok(bolt_ast::Visibility::Package),
                        "module" => Ok(bolt_ast::Visibility::Module),
                        "test" => Ok(bolt_ast::Visibility::Test),
                        "friend" => {
                            // pub(friend: Type1, Type2)
                            self.consume(&TokenType::Colon, "expected ':' after 'friend'")?;
                            let friends = self.parse_comma_separated(|parser| {
                                if let TokenType::Identifier(name) = &parser.peek().token_type {
                                    let name = name.clone();
                                    parser.advance();
                                    Ok(name)
                                } else {
                                    Err(ParseError::expected_token(
                                        "friend type name",
                                        parser.peek(),
                                    ))
                                }
                            })?;
                            Ok(bolt_ast::Visibility::Friend(friends))
                        }
                        _ => Err(ParseError::invalid_visibility(
                            &format!("unknown visibility modifier '{}'", modifier),
                            self.current_span(),
                        )),
                    }
                } else {
                    Ok(bolt_ast::Visibility::Public)
                }
            }
            TokenType::Protected => {
                self.advance();
                Ok(bolt_ast::Visibility::Protected)
            }
            TokenType::Internal => {
                self.advance();
                Ok(bolt_ast::Visibility::Internal)
            }
            _ => Ok(bolt_ast::Visibility::Private), // Default visibility
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use bolt_lexer::Lexer;

    fn create_type_parser(source: &str) -> Parser {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize test input");
        Parser::new(tokens)
    }

    #[test]
    fn test_primitive_types() {
        let mut parser = create_type_parser("Int");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Int);

        let mut parser = create_type_parser("String");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::String);

        let mut parser = create_type_parser("Bool");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_array_type() {
        let mut parser = create_type_parser("[Int]");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Array(Box::new(Type::Int)));

        let mut parser = create_type_parser("[String]");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Array(Box::new(Type::String)));
    }

    #[test]
    fn test_map_type() {
        let mut parser = create_type_parser("{String: Int}");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Map(Box::new(Type::String), Box::new(Type::Int)));
    }

    #[test]
    fn test_function_type() {
        let mut parser = create_type_parser("Int -> String");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::Function {
                params: vec![Type::Int],
                return_type: Box::new(Type::String),
            }
        );

        let mut parser = create_type_parser("(Int, String) -> Bool");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::Function {
                params: vec![Type::Int, Type::String],
                return_type: Box::new(Type::Bool),
            }
        );
    }

    #[test]
    fn test_reference_types() {
        let mut parser = create_type_parser("&Int");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::Reference {
                mutable: false,
                inner: Box::new(Type::Int),
            }
        );

        let mut parser = create_type_parser("&mut String");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::Reference {
                mutable: true,
                inner: Box::new(Type::String),
            }
        );
    }

    #[test]
    fn test_generic_types() {
        let mut parser = create_type_parser("Vec<Int>");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::UserDefined {
                name: "Vec".to_string(),
                generics: vec![Type::Int],
            }
        );
    }

    #[test]
    fn test_result_type() {
        let mut parser = create_type_parser("Result<Int, String>");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::Result(Box::new(Type::Int), Box::new(Type::String))
        );
    }

    #[test]
    fn test_maybe_type() {
        let mut parser = create_type_parser("Maybe<String>");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Maybe(Box::new(Type::String)));
    }

    #[test]
    fn test_unit_type() {
        let mut parser = create_type_parser("()");
        let ty = parser.parse_type().unwrap();
        assert_eq!(ty, Type::Unit);
    }

    #[test]
    fn test_complex_nested_type() {
        let mut parser = create_type_parser("Result<[Int], String>");
        let ty = parser.parse_type().unwrap();
        assert_eq!(
            ty,
            Type::Result(
                Box::new(Type::Array(Box::new(Type::Int))),
                Box::new(Type::String)
            )
        );
    }

    #[test]
    fn test_type_annotation() {
        let mut parser = create_type_parser(":: Int");
        let annotation = parser.parse_type_annotation().unwrap();
        assert_eq!(annotation.explicit_type, Some(Type::Int));

        let mut parser = create_type_parser("x"); // No :: prefix
        let annotation = parser.parse_type_annotation().unwrap();
        assert_eq!(annotation.explicit_type, None);
    }

    #[test]
    fn test_generic_params() {
        let mut parser = create_type_parser("<T, U: Clone>");
        let params = parser.parse_generic_params().unwrap();

        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "T");
        assert!(params[0].bounds.is_empty());

        assert_eq!(params[1].name, "U");
        assert_eq!(params[1].bounds.len(), 1);
        assert_eq!(params[1].bounds[0].name, "Clone");
    }

    #[test]
    fn test_visibility_parsing() {
        let mut parser = create_type_parser("pub");
        let vis = parser.parse_visibility().unwrap();
        assert_eq!(vis, bolt_ast::Visibility::Public);

        let mut parser = create_type_parser("protected");
        let vis = parser.parse_visibility().unwrap();
        assert_eq!(vis, bolt_ast::Visibility::Protected);

        let mut parser = create_type_parser("let"); // No visibility modifier
        let vis = parser.parse_visibility().unwrap();
        assert_eq!(vis, bolt_ast::Visibility::Private);
    }

    #[test]
    fn test_error_cases() {
        // Unclosed array type
        let mut parser = create_type_parser("[Int");
        assert!(parser.parse_type().is_err());

        // Invalid map type
        let mut parser = create_type_parser("{Int Int}");
        assert!(parser.parse_type().is_err());

        // Result without type parameters
        let mut parser = create_type_parser("Result");
        assert!(parser.parse_type().is_err());
    }
}

