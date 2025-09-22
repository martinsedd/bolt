use crate::error::{ParseError, ParseResult};
use crate::parser::Parser;
use bolt_ast::{
    AccessorDef, ClassBody, ClassField, ClassMethod, Constructor, Parameter, Stmt, Visibility,
};
use bolt_lexer::{Position, TokenType};

impl Parser {
    /// Parse a statement
    pub fn parse_statement(&mut self) -> ParseResult<Stmt> {
        // Skip any leading trivia
        self.skip_trivia();

        let start_pos = self.current_position();

        // Check for visibility modifiers first
        let visibility = self.parse_visibility()?;

        match &self.peek().token_type {
            // Variable declarations: let x = 42, mut y :: Int = 10
            TokenType::Let | TokenType::Mut => {
                self.parse_variable_declaration(visibility, start_pos)
            }

            // Function definitions: fn name() : body end
            TokenType::Fn => self.parse_function_definition(visibility, start_pos),

            // Class definitions: class Name : ... end
            TokenType::Class => self.parse_class_definition(visibility, start_pos),

            // Struct definitions: struct Name : ... end
            TokenType::Struct => self.parse_struct_definition(visibility, start_pos),

            // Enum definitions: enum Name : ... end
            TokenType::Enum => self.parse_enum_definition(visibility, start_pos),

            // Trait definitions: trait Name : ... end
            TokenType::Trait => self.parse_trait_definition(visibility, start_pos),

            // Implementation blocks: impl TraitName for TypeName : ... end
            TokenType::Impl => self.parse_impl_block(start_pos),

            // Module definitions: module Name : ... end
            TokenType::Module => self.parse_module_definition(visibility, start_pos),

            // Import statements: import ModuleName
            TokenType::Import => self.parse_import_statement(visibility, start_pos),

            // Control flow statements
            TokenType::If => self.parse_if_statement(start_pos),
            TokenType::Loop => self.parse_loop_statement(start_pos),
            TokenType::Match => self.parse_match_statement(start_pos),
            TokenType::Return => self.parse_return_statement(start_pos),
            TokenType::Break => self.parse_break_statement(start_pos),
            TokenType::Continue => self.parse_continue_statement(start_pos),
            TokenType::Defer => self.parse_defer_statement(start_pos),

            // Block statements: { ... }
            TokenType::LeftBrace => self.parse_block_statement(start_pos),

            _ => {
                // If we have a visibility modifier but no recognizable statement, error
                if !matches!(visibility, Visibility::Private) {
                    return Err(ParseError::invalid_statement(
                        "visibility modifier can only be used with declarations",
                        self.current_span(),
                    ));
                }

                // Otherwise, try to parse as expression statement
                self.parse_expression_statement(start_pos)
            }
        }
    }

    /// Parse variable declaration: let x = 42, mut y :: Int = 10
    fn parse_variable_declaration(
        &mut self,
        _visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        let mutable = if self.match_token(&TokenType::Mut) {
            true
        } else if self.match_token(&TokenType::Let) {
            false
        } else {
            return Err(ParseError::expected_token("'let' or 'mut'", self.peek()));
        };

        // Parse variable name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("variable name", self.peek()));
        };

        // Parse optional type annotation
        let type_annotation = self.parse_type_annotation()?;

        // Parse optional initializer
        let initializer = if self.match_token(&TokenType::Equal) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        let span = self.span_from(start_pos);

        Ok(Stmt::VariableDecl {
            name,
            mutable,
            type_annotation,
            initializer,
            span,
        })
    }

    /// Parse function definition: fn name(params) -> RetType : body end
    fn parse_function_definition(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Fn, "expected 'fn'")?;

        // Check for modifiers
        let is_pure = if self.match_token(&TokenType::Pure) {
            true
        } else {
            false
        };

        let is_async = if self.match_token(&TokenType::Async) {
            true
        } else {
            false
        };

        // Parse function name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("function name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse parameters
        self.consume(&TokenType::LeftParen, "expected '(' after function name")?;
        let params = if self.check(&TokenType::RightParen) {
            vec![]
        } else {
            self.parse_comma_separated(|parser| parser.parse_parameter())?
        };
        self.consume(&TokenType::RightParen, "expected ')' after parameters")?;

        // Parse return type
        let return_type = if self.match_token(&TokenType::FunctionArrow) {
            let ty = self.parse_type()?;
            bolt_ast::TypeAnnotation::explicit(ty, self.current_span())
        } else {
            // Default to unit type
            bolt_ast::TypeAnnotation::explicit(bolt_ast::Type::Unit, self.current_span())
        };

        // Parse body
        let body = self.parse_block(|parser| parser.parse_expression())?;

        let span = self.span_from(start_pos);

        Ok(Stmt::FunctionDef {
            name,
            is_pure,
            is_async,
            generics,
            params,
            return_type,
            body: Box::new(body),
            visibility,
            span,
        })
    }

    /// Parse a function parameter: name :: Type
    fn parse_parameter(&mut self) -> ParseResult<Parameter> {
        let start_pos = self.current_position();

        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("parameter name", self.peek()));
        };

        let type_annotation = self.parse_type_annotation()?;
        let span = self.span_from(start_pos);

        Ok(Parameter {
            name,
            type_annotation,
            span,
        })
    }

    /// Parse class definition: class Name : ... end
    fn parse_class_definition(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Class, "expected 'class'")?;

        // Parse class name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("class name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse optional superclass: :: SuperClass
        let superclass = if self.match_token(&TokenType::DoubleColon) {
            if let TokenType::Identifier(super_name) = &self.peek().token_type {
                let super_name = super_name.clone();
                self.advance();
                Some(super_name)
            } else {
                return Err(ParseError::expected_token("superclass name", self.peek()));
            }
        } else {
            None
        };

        // Parse traits (not implemented yet)
        let traits = vec![]; // TODO: Parse trait implementations

        // Parse class body
        let body = self.parse_block(|parser| parser.parse_class_body())?;

        let span = self.span_from(start_pos);

        Ok(Stmt::ClassDef {
            name,
            generics,
            superclass,
            traits,
            body,
            visibility,
            span,
        })
    }

    /// Parse class body contents
    fn parse_class_body(&mut self) -> ParseResult<ClassBody> {
        let mut fields = vec![];
        let mut methods = vec![];
        let mut constructors = vec![];

        while !self.check(&TokenType::End) && !self.is_at_end() {
            self.skip_trivia();

            if self.check(&TokenType::End) {
                break;
            }

            // Parse visibility
            let visibility = self.parse_visibility()?;

            match &self.peek().token_type {
                TokenType::Identifier(_) => {
                    // Could be field or method - look ahead
                    if self.looks_like_field() {
                        fields.push(self.parse_class_field(visibility)?);
                    } else {
                        methods.push(self.parse_class_method(visibility)?);
                    }
                }
                TokenType::Init => {
                    constructors.push(self.parse_constructor()?);
                }
                _ => {
                    return Err(ParseError::invalid_statement(
                        "expected field, method, or constructor in class body",
                        self.current_span(),
                    ));
                }
            }
        }

        Ok(ClassBody {
            fields,
            methods,
            constructors,
        })
    }

    /// Check if the current position looks like a field declaration
    fn looks_like_field(&mut self) -> bool {
        if matches!(self.peek().token_type, TokenType::Identifier(_)) {
            let next_token = self.peek_ahead(1);
            matches!(
                next_token.token_type,
                TokenType::DoubleColon | TokenType::LeftBrace
            )
        } else {
            false
        }
    }

    /// Parse class field: name :: Type { get, set }
    fn parse_class_field(&mut self, visibility: Visibility) -> ParseResult<ClassField> {
        let start_pos = self.current_position();

        let mutable = self.match_token(&TokenType::Mut);

        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("field name", self.peek()));
        };

        let type_annotation = self.parse_type_annotation()?;

        // Parse optional accessors: { get, set }
        let (getter, setter) = if self.match_token(&TokenType::LeftBrace) {
            let mut getter = None;
            let mut setter = None;

            while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                if let TokenType::Identifier(accessor) = &self.peek().token_type {
                    match accessor.as_str() {
                        "get" => {
                            self.advance();
                            getter = Some(AccessorDef {
                                body: None, // Auto-generated getter
                                span: self.current_span(),
                            });
                        }
                        "set" => {
                            self.advance();
                            setter = Some(AccessorDef {
                                body: None, // Auto-generated setter
                                span: self.current_span(),
                            });
                        }
                        _ => {
                            return Err(ParseError::invalid_statement(
                                "expected 'get' or 'set' in accessor block",
                                self.current_span(),
                            ));
                        }
                    }
                } else {
                    break;
                }

                // Optional comma
                self.match_token(&TokenType::Comma);
            }

            self.consume(&TokenType::RightBrace, "expected '}' after accessors")?;
            (getter, setter)
        } else {
            (None, None)
        };

        let span = self.span_from(start_pos);

        Ok(ClassField {
            name,
            mutable,
            type_annotation,
            visibility,
            getter,
            setter,
            span,
        })
    }

    /// Parse class method (simplified for now)
    fn parse_class_method(&mut self, visibility: Visibility) -> ParseResult<ClassMethod> {
        let start_pos = self.current_position();

        let is_override = self.match_token(&TokenType::Override);
        let is_pure = self.match_token(&TokenType::Pure);
        let is_async = self.match_token(&TokenType::Async);

        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("method name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse parameters
        self.consume(&TokenType::LeftParen, "expected '(' after method name")?;
        let params = if self.check(&TokenType::RightParen) {
            vec![]
        } else {
            self.parse_comma_separated(|parser| parser.parse_parameter())?
        };
        self.consume(&TokenType::RightParen, "expected ')' after parameters")?;

        // Parse return type
        let return_type = if self.match_token(&TokenType::FunctionArrow) {
            self.parse_type_annotation()?
        } else {
            bolt_ast::TypeAnnotation::explicit(bolt_ast::Type::Unit, self.current_span())
        };

        // Parse body
        let body = self.parse_block(|parser| parser.parse_expression())?;

        let span = self.span_from(start_pos);

        Ok(ClassMethod {
            name,
            is_pure,
            is_async,
            is_override,
            generics,
            params,
            return_type,
            body: Box::new(body),
            visibility,
            span,
        })
    }

    /// Parse constructor (simplified)
    fn parse_constructor(&mut self) -> ParseResult<Constructor> {
        let start_pos = self.current_position();

        self.consume(&TokenType::Init, "expected 'init'")?;

        // Parse parameters
        self.consume(&TokenType::LeftParen, "expected '(' after 'init'")?;
        let params = if self.check(&TokenType::RightParen) {
            vec![]
        } else {
            self.parse_comma_separated(|parser| parser.parse_parameter())?
        };
        self.consume(&TokenType::RightParen, "expected ')' after parameters")?;

        // Parse optional body
        let body = if self.check(&TokenType::Colon) {
            Some(Box::new(
                self.parse_block(|parser| parser.parse_expression())?,
            ))
        } else {
            None // Auto-generated constructor
        };

        let span = self.span_from(start_pos);

        Ok(Constructor { params, body, span })
    }

    /// Parse expression statement
    fn parse_expression_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        let expr = self.parse_expression()?;
        let span = self.span_from(start_pos);

        Ok(Stmt::ExpressionStmt {
            expr: Box::new(expr),
            span,
        })
    }

    /// Parse return statement: return expr
    fn parse_return_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Return, "expected 'return'")?;

        let value =
            if self.check(&TokenType::End) || self.check(&TokenType::Newline) || self.is_at_end() {
                None
            } else {
                Some(Box::new(self.parse_expression()?))
            };

        let span = self.span_from(start_pos);

        Ok(Stmt::Return { value, span })
    }

    // Placeholder implementations for complex statement types

    fn parse_struct_definition(
        &mut self,
        _visibility: Visibility,
        _start_pos: Position,
    ) -> ParseResult<Stmt> {
        todo!("Struct definition parsing")
    }

    fn parse_enum_definition(
        &mut self,
        _visibility: Visibility,
        _start_pos: Position,
    ) -> ParseResult<Stmt> {
        todo!("Enum definition parsing")
    }

    fn parse_trait_definition(
        &mut self,
        _visibility: Visibility,
        _start_pos: Position,
    ) -> ParseResult<Stmt> {
        todo!("Trait definition parsing")
    }

    fn parse_impl_block(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Implementation block parsing")
    }

    fn parse_module_definition(
        &mut self,
        _visibility: Visibility,
        _start_pos: Position,
    ) -> ParseResult<Stmt> {
        todo!("Module definition parsing")
    }

    fn parse_import_statement(
        &mut self,
        _visibility: Visibility,
        _start_pos: Position,
    ) -> ParseResult<Stmt> {
        todo!("Import statement parsing")
    }

    fn parse_if_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("If statement parsing")
    }

    fn parse_loop_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Loop statement parsing")
    }

    fn parse_match_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Match statement parsing")
    }

    fn parse_break_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Break statement parsing")
    }

    fn parse_continue_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Continue statement parsing")
    }

    fn parse_defer_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Defer statement parsing")
    }

    fn parse_block_statement(&mut self, _start_pos: Position) -> ParseResult<Stmt> {
        todo!("Block statement parsing")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use bolt_lexer::Lexer;

    fn create_stmt_parser(source: &str) -> Parser {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize test input");
        Parser::new(tokens)
    }

    #[test]
    fn test_variable_declaration() {
        let mut parser = create_stmt_parser("let x = 42");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::VariableDecl {
            name,
            mutable,
            initializer,
            ..
        } = stmt
        {
            assert_eq!(name, "x");
            assert!(!mutable);
            assert!(initializer.is_some());
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_mutable_variable() {
        let mut parser = create_stmt_parser("mut y :: Int = 10");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::VariableDecl { name, mutable, .. } = stmt {
            assert_eq!(name, "y");
            assert!(mutable);
        } else {
            panic!("Expected mutable variable declaration");
        }
    }

    #[test]
    fn test_function_definition() {
        let mut parser = create_stmt_parser("fn add(a :: Int, b :: Int) -> Int : a + b end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::FunctionDef { name, params, .. } = stmt {
            assert_eq!(name, "add");
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "a");
            assert_eq!(params[1].name, "b");
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    fn test_simple_function() {
        let mut parser = create_stmt_parser("fn hello() : print(\"Hello\") end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::FunctionDef { name, params, .. } = stmt {
            assert_eq!(name, "hello");
            assert_eq!(params.len(), 0);
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    fn test_class_definition() {
        let mut parser = create_stmt_parser("class User : name :: String { get } end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::ClassDef { name, body, .. } = stmt {
            assert_eq!(name, "User");
            assert_eq!(body.fields.len(), 1);
            assert_eq!(body.fields[0].name, "name");
        } else {
            panic!("Expected class definition");
        }
    }

    #[test]
    fn test_expression_statement() {
        let mut parser = create_stmt_parser("func()");
        let stmt = parser.parse_statement().unwrap();

        assert!(matches!(stmt, Stmt::ExpressionStmt { .. }));
    }

    #[test]
    fn test_return_statement() {
        let mut parser = create_stmt_parser("return 42");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::Return { value, .. } = stmt {
            assert!(value.is_some());
        } else {
            panic!("Expected return statement");
        }
    }

    #[test]
    fn test_visibility_modifiers() {
        let mut parser = create_stmt_parser("pub fn public_func() : 42 end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::FunctionDef { visibility, .. } = stmt {
            assert!(matches!(visibility, Visibility::Public));
        } else {
            panic!("Expected function with public visibility");
        }
    }

    #[test]
    fn test_parameter_parsing() {
        let mut parser = create_stmt_parser("fn test(x :: Int, y :: String) : x end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::FunctionDef { params, .. } = stmt {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "x");
            assert_eq!(params[1].name, "y");
        } else {
            panic!("Expected function with parameters");
        }
    }
}
