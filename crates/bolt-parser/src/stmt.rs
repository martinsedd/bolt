use crate::error::{ParseError, ParseResult};
use crate::parser::Parser;
use bolt_ast::{
    AccessorDef, ClassBody, ClassField, ClassMethod, Constructor, EnumVariant, EnumVariantData,
    ImplItem, ImportPath, LoopKind, MatchArm, Parameter, Pattern, Stmt, StructField, TraitBody,
    TraitMethod, Visibility,
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

    /// Parse struct definition: struct Name : field1 :: Type field2 :: Type end
    fn parse_struct_definition(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Struct, "expected 'struct'")?;

        // Parse struct name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("struct name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse struct body
        let fields = self.parse_block(|parser| {
            let mut fields = vec![];

            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                parser.skip_trivia();

                if parser.check(&TokenType::End) {
                    break;
                }

                let field_start = parser.current_position();
                let field_visibility = parser.parse_visibility()?;

                let field_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                    let name = name.clone();
                    parser.advance();
                    name
                } else {
                    return Err(ParseError::expected_token("field name", parser.peek()));
                };

                let type_annotation = parser.parse_type_annotation()?;
                let field_span = parser.span_from(field_start);

                fields.push(StructField {
                    name: field_name,
                    type_annotation,
                    visibility: field_visibility,
                    span: field_span,
                });
            }

            Ok(fields)
        })?;

        let span = self.span_from(start_pos);

        Ok(Stmt::StructDef {
            name,
            generics,
            fields,
            visibility,
            span,
        })
    }

    /// Parse enum definition: enum Name : Variant1 Variant2(Type) end
    fn parse_enum_definition(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Enum, "expected 'enum'")?;

        // Parse enum name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("enum name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse enum variants
        let variants = self.parse_block(|parser| {
            let mut variants = vec![];

            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                parser.skip_trivia();

                if parser.check(&TokenType::End) {
                    break;
                }

                let variant_start = parser.current_position();

                let variant_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                    let name = name.clone();
                    parser.advance();
                    name
                } else {
                    return Err(ParseError::expected_token("variant name", parser.peek()));
                };

                let data = if parser.match_token(&TokenType::LeftParen) {
                    // Tuple variant: RGB(Int, Int, Int)
                    let types = if parser.check(&TokenType::RightParen) {
                        vec![]
                    } else {
                        parser.parse_comma_separated(|p| p.parse_type())?
                    };
                    parser.consume(&TokenType::RightParen, "expected ')' after variant types")?;
                    EnumVariantData::Tuple(types)
                } else if parser.match_token(&TokenType::LeftBrace) {
                    // Struct variant: Point { x: Int, y: Int }
                    let fields = if parser.check(&TokenType::RightBrace) {
                        vec![]
                    } else {
                        parser.parse_comma_separated(|p| {
                            let field_start = p.current_position();
                            let field_name =
                                if let TokenType::Identifier(name) = &p.peek().token_type {
                                    let name = name.clone();
                                    p.advance();
                                    name
                                } else {
                                    return Err(ParseError::expected_token("field name", p.peek()));
                                };

                            let type_annotation = p.parse_type_annotation()?;
                            let field_span = p.span_from(field_start);

                            Ok(StructField {
                                name: field_name,
                                type_annotation,
                                visibility: Visibility::Public, // Enum fields are public
                                span: field_span,
                            })
                        })?
                    };
                    parser.consume(&TokenType::RightBrace, "expected '}' after variant fields")?;
                    EnumVariantData::Struct(fields)
                } else {
                    // Unit variant: Red
                    EnumVariantData::Unit
                };

                let variant_span = parser.span_from(variant_start);

                variants.push(EnumVariant {
                    name: variant_name,
                    data,
                    span: variant_span,
                });
            }

            Ok(variants)
        })?;

        let span = self.span_from(start_pos);

        Ok(Stmt::EnumDef {
            name,
            generics,
            variants,
            visibility,
            span,
        })
    }

    /// Parse trait definition: trait Name : fn method() -> Type end
    fn parse_trait_definition(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Trait, "expected 'trait'")?;

        // Parse trait name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("trait name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse supertraits: : SuperTrait1 + SuperTrait2
        let supertraits = if self.match_token(&TokenType::Colon) {
            let mut traits = vec![];

            loop {
                if let TokenType::Identifier(trait_name) = &self.peek().token_type {
                    traits.push(trait_name.clone());
                    self.advance();
                } else {
                    return Err(ParseError::expected_token("trait name", self.peek()));
                }

                if !self.match_token(&TokenType::Plus) {
                    break;
                }
            }

            traits
        } else {
            vec![]
        };

        // Parse trait body
        let body = self.parse_block(|parser| {
            let mut methods = vec![];

            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                parser.skip_trivia();

                if parser.check(&TokenType::End) {
                    break;
                }

                // Parse trait method
                let method_start = parser.current_position();

                parser.consume(&TokenType::Fn, "expected 'fn' in trait method")?;

                let method_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                    let name = name.clone();
                    parser.advance();
                    name
                } else {
                    return Err(ParseError::expected_token("method name", parser.peek()));
                };

                // Parse generic parameters
                let method_generics = parser.parse_generic_params()?;

                // Parse parameters
                parser.consume(&TokenType::LeftParen, "expected '(' after method name")?;
                let params = if parser.check(&TokenType::RightParen) {
                    vec![]
                } else {
                    parser.parse_comma_separated(|p| p.parse_parameter())?
                };
                parser.consume(&TokenType::RightParen, "expected ')' after parameters")?;

                // Parse return type
                let return_type = if parser.match_token(&TokenType::FunctionArrow) {
                    let ty = parser.parse_type()?;
                    bolt_ast::TypeAnnotation::explicit(ty, parser.current_span())
                } else {
                    bolt_ast::TypeAnnotation::explicit(bolt_ast::Type::Unit, parser.current_span())
                };

                // Parse optional default implementation
                let default_body = if parser.check(&TokenType::Colon) {
                    Some(Box::new(parser.parse_block(|p| p.parse_expression())?))
                } else {
                    None
                };

                let method_span = parser.span_from(method_start);

                methods.push(TraitMethod {
                    name: method_name,
                    generics: method_generics,
                    params,
                    return_type,
                    default_body,
                    span: method_span,
                });
            }

            Ok(TraitBody { methods })
        })?;

        let span = self.span_from(start_pos);

        Ok(Stmt::TraitDef {
            name,
            generics,
            supertraits,
            body,
            visibility,
            span,
        })
    }

    /// Parse implementation block: impl TraitName for TypeName : ... end
    fn parse_impl_block(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Impl, "expected 'impl'")?;

        // Parse optional trait name
        let trait_name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();

            // Check if this is "impl Trait for Type" or "impl Type"
            if self.match_token(&TokenType::For) {
                Some(name)
            } else {
                // This was actually the type name, backtrack conceptually
                let type_name = name;
                let generics = self.parse_generic_params()?;

                // Parse impl body
                let body = self.parse_impl_body()?;

                let span = self.span_from(start_pos);

                return Ok(Stmt::ImplBlock {
                    trait_name: None,
                    type_name,
                    generics,
                    body,
                    span,
                });
            }
        } else {
            return Err(ParseError::expected_token(
                "trait or type name",
                self.peek(),
            ));
        };

        // Parse type name (for "impl Trait for Type")
        let type_name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("type name", self.peek()));
        };

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Parse impl body
        let body = self.parse_impl_body()?;

        let span = self.span_from(start_pos);

        Ok(Stmt::ImplBlock {
            trait_name,
            type_name,
            generics,
            body,
            span,
        })
    }

    /// Parse implementation body items
    fn parse_impl_body(&mut self) -> ParseResult<Vec<ImplItem>> {
        self.parse_block(|parser| {
            let mut items = vec![];

            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                parser.skip_trivia();

                if parser.check(&TokenType::End) {
                    break;
                }

                let item_start = parser.current_position();
                let visibility = parser.parse_visibility()?;

                match &parser.peek().token_type {
                    TokenType::Fn => {
                        // Method implementation
                        let method = parser.parse_class_method(visibility)?;
                        items.push(ImplItem::Method(method));
                    }
                    TokenType::Type => {
                        // Associated type
                        parser.advance(); // consume 'type'

                        let name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                            let name = name.clone();
                            parser.advance();
                            name
                        } else {
                            return Err(ParseError::expected_token("type name", parser.peek()));
                        };

                        parser.consume(
                            &TokenType::Equal,
                            "expected '=' after associated type name",
                        )?;
                        let type_def = parser.parse_type()?;

                        let span = parser.span_from(item_start);

                        items.push(ImplItem::Type {
                            name,
                            type_def,
                            span,
                        });
                    }
                    TokenType::Const => {
                        // Associated constant
                        parser.advance(); // consume 'const'

                        let name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                            let name = name.clone();
                            parser.advance();
                            name
                        } else {
                            return Err(ParseError::expected_token("constant name", parser.peek()));
                        };

                        let type_annotation = parser.parse_type_annotation()?;
                        parser.consume(&TokenType::Equal, "expected '=' after constant type")?;
                        let value = parser.parse_expression()?;

                        let span = parser.span_from(item_start);

                        items.push(ImplItem::Const {
                            name,
                            type_annotation,
                            value: Box::new(value),
                            span,
                        });
                    }
                    _ => {
                        return Err(ParseError::invalid_statement(
                            "expected method, type, or const in impl block",
                            parser.current_span(),
                        ));
                    }
                }
            }

            Ok(items)
        })
    }

    /// Parse module definition: module Name : statements... end
    fn parse_module_definition(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Module, "expected 'module'")?;

        // Parse module name
        let name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("module name", self.peek()));
        };

        // Parse module body
        let body = self.parse_block(|parser| {
            let mut statements = vec![];

            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                statements.push(Box::new(parser.parse_statement()?));
                parser.skip_trivia();
            }

            Ok(statements)
        })?;

        let span = self.span_from(start_pos);

        Ok(Stmt::ModuleDef {
            name,
            body,
            visibility,
            span,
        })
    }

    /// Parse import statement: import ModuleName, import ModuleName::Item
    fn parse_import_statement(
        &mut self,
        visibility: Visibility,
        start_pos: Position,
    ) -> ParseResult<Stmt> {
        self.consume(&TokenType::Import, "expected 'import'")?;

        // Parse import path
        let path = self.parse_import_path()?;

        // Parse optional alias: as Alias
        let alias = if self.match_token(&TokenType::As) {
            if let TokenType::Identifier(name) = &self.peek().token_type {
                let name = name.clone();
                self.advance();
                Some(name)
            } else {
                return Err(ParseError::expected_token("alias name", self.peek()));
            }
        } else {
            None
        };

        let span = self.span_from(start_pos);

        Ok(Stmt::Import {
            path,
            alias,
            visibility,
            span,
        })
    }

    /// Parse import path: Module, Module::Item, Module::*, Module::{Item1, Item2}
    fn parse_import_path(&mut self) -> ParseResult<ImportPath> {
        let module_name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token("module name", self.peek()));
        };

        if !self.match_token(&TokenType::DoubleColon) {
            // Simple import: Module
            return Ok(ImportPath::Simple(module_name));
        }

        match &self.peek().token_type {
            TokenType::Star => {
                // Glob import: Module::*
                self.advance();
                Ok(ImportPath::Glob(module_name))
            }
            TokenType::LeftBrace => {
                // Multiple imports: Module::{Item1, Item2}
                self.advance(); // consume '{'

                let items = self.parse_comma_separated(|parser| {
                    if let TokenType::Identifier(name) = &parser.peek().token_type {
                        let name = name.clone();
                        parser.advance();
                        Ok(name)
                    } else {
                        Err(ParseError::expected_token(
                            "import item name",
                            parser.peek(),
                        ))
                    }
                })?;

                self.consume(&TokenType::RightBrace, "expected '}' after import items")?;
                Ok(ImportPath::Multiple(module_name, items))
            }
            TokenType::Identifier(item_name) => {
                // Nested import: Module::Item
                let item_name = item_name.clone();
                self.advance();
                Ok(ImportPath::Nested(module_name, item_name))
            }
            _ => Err(ParseError::expected_token("import item", self.peek())),
        }
    }

    /// Parse if statement: if condition : then_branch else : else_branch end
    fn parse_if_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::If, "expected 'if'")?;

        let condition = self.parse_expression()?;
        self.consume(&TokenType::Colon, "expected ':' after if condition")?;

        // Parse then branch statements (terminated by 'else' or 'end')
        let mut then_statements = vec![];
        while !self.check(&TokenType::Else) && !self.check(&TokenType::End) && !self.is_at_end() {
            then_statements.push(Box::new(self.parse_statement()?));
            self.skip_trivia();
        }

        // Convert statements to a single statement
        let then_stmt = if then_statements.len() == 1 {
            *then_statements.into_iter().next().unwrap()
        } else {
            Stmt::Block {
                statements: then_statements,
                span: self.span_from(start_pos),
            }
        };

        let else_branch = if self.match_token(&TokenType::Else) {
            if self.check(&TokenType::If) {
                // else if - parse as another if statement
                Some(Box::new(self.parse_if_statement(self.current_position())?))
            } else {
                // else block
                self.consume(&TokenType::Colon, "expected ':' after else")?;

                let mut else_statements = vec![];
                while !self.check(&TokenType::End) && !self.is_at_end() {
                    else_statements.push(Box::new(self.parse_statement()?));
                    self.skip_trivia();
                }

                let else_stmt = if else_statements.len() == 1 {
                    *else_statements.into_iter().next().unwrap()
                } else {
                    Stmt::Block {
                        statements: else_statements,
                        span: self.span_from(start_pos),
                    }
                };
                Some(Box::new(else_stmt))
            }
        } else {
            None
        };

        self.consume(&TokenType::End, "expected 'end' after if statement")?;
        let span = self.span_from(start_pos);

        Ok(Stmt::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_stmt),
            else_branch,
            span,
        })
    }
    /// Parse loop statement: loop : body end, loop (condition) : body end, etc.
    fn parse_loop_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Loop, "expected 'loop'")?;

        let kind = if self.match_token(&TokenType::LeftParen) {
            // Check what kind of loop this is
            if self.check(&TokenType::Let) {
                // For loop: loop (let i = 0; i < 10; i++) : body end
                let init = Some(Box::new(self.parse_statement()?));
                self.consume(&TokenType::Semicolon, "expected ';' after loop init")?;

                let condition = if self.check(&TokenType::Semicolon) {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                self.consume(&TokenType::Semicolon, "expected ';' after loop condition")?;

                let increment = if self.check(&TokenType::RightParen) {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };

                self.consume(&TokenType::RightParen, "expected ')' after loop increment")?;

                LoopKind::For {
                    init,
                    condition,
                    increment,
                }
            } else {
                // While loop: loop (condition) : body end
                let condition = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "expected ')' after loop condition")?;
                LoopKind::While(Box::new(condition))
            }
        } else if self.peek_ahead(1).token_type == TokenType::Colon {
            // Check if this looks like foreach: loop collection : body end
            let iterable = self.parse_expression()?;
            // For now, treat as foreach with implicit variable
            LoopKind::ForEach {
                variable: "item".to_string(), // Default variable name
                iterable: Box::new(iterable),
            }
        } else {
            // Infinite loop: loop : body end
            LoopKind::Infinite
        };

        let body = self.parse_block(|parser| {
            let mut statements = vec![];
            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                statements.push(Box::new(parser.parse_statement()?));
                parser.skip_trivia();
            }

            if statements.len() == 1 {
                Ok(*statements.into_iter().next().unwrap())
            } else {
                Ok(Stmt::Block {
                    statements,
                    span: parser.span_from(parser.current_position()),
                })
            }
        })?;

        let span = self.span_from(start_pos);

        Ok(Stmt::Loop {
            kind,
            body: Box::new(body),
            span,
        })
    }

    /// Parse match statement: match expr : pattern => stmt ... end
    fn parse_match_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Match, "expected 'match'")?;

        let expr = self.parse_expression()?;

        let arms = self.parse_block(|parser| {
            let mut arms = vec![];

            while !parser.check(&TokenType::End) && !parser.is_at_end() {
                parser.skip_trivia();

                if parser.check(&TokenType::End) {
                    break;
                }

                let arm_start = parser.current_position();

                // Parse pattern
                let pattern = parser.parse_pattern()?;

                // Parse optional guard: if condition
                let guard = if parser.match_token(&TokenType::If) {
                    Some(Box::new(parser.parse_expression()?))
                } else {
                    None
                };

                parser.consume(&TokenType::Arrow, "expected '=>' after match pattern")?;

                // Parse body statement
                let body = Box::new(parser.parse_statement()?);

                let arm_span = parser.span_from(arm_start);

                arms.push(MatchArm {
                    pattern,
                    guard,
                    body,
                    span: arm_span,
                });

                parser.skip_trivia();
            }

            Ok(arms)
        })?;

        let span = self.span_from(start_pos);

        Ok(Stmt::Match {
            expr: Box::new(expr),
            arms,
            span,
        })
    }

    /// Parse pattern for match expressions
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let start_pos = self.current_position();

        match &self.peek().token_type {
            // Wildcard pattern: _
            TokenType::Identifier(name) if name == "_" => {
                self.advance();
                Ok(Pattern::Wildcard(self.span_from(start_pos)))
            }

            // Variable or constructor pattern
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance();

                if self.match_token(&TokenType::LeftParen) {
                    // Constructor pattern: Some(x)
                    let args = if self.check(&TokenType::RightParen) {
                        vec![]
                    } else {
                        self.parse_comma_separated(|parser| parser.parse_pattern())?
                    };
                    self.consume(
                        &TokenType::RightParen,
                        "expected ')' after constructor arguments",
                    )?;

                    Ok(Pattern::Constructor {
                        name,
                        args,
                        span: self.span_from(start_pos),
                    })
                } else {
                    // Variable pattern: x
                    Ok(Pattern::Variable {
                        name,
                        span: self.span_from(start_pos),
                    })
                }
            }

            // Literal patterns
            TokenType::Integer(n) => {
                let value = *n;
                self.advance();
                Ok(Pattern::Literal {
                    value: bolt_ast::LiteralValue::Integer(value),
                    span: self.span_from(start_pos),
                })
            }

            TokenType::String(s) => {
                let value = s.clone();
                self.advance();
                Ok(Pattern::Literal {
                    value: bolt_ast::LiteralValue::String(value),
                    span: self.span_from(start_pos),
                })
            }

            TokenType::Boolean(b) => {
                let value = *b;
                self.advance();
                Ok(Pattern::Literal {
                    value: bolt_ast::LiteralValue::Boolean(value),
                    span: self.span_from(start_pos),
                })
            }

            _ => Err(ParseError::invalid_pattern(
                "expected pattern",
                self.current_span(),
            )),
        }
    }

    /// Parse break statement: break or break label
    fn parse_break_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Break, "expected 'break'")?;

        let label = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            Some(name)
        } else {
            None
        };

        let span = self.span_from(start_pos);

        Ok(Stmt::Break { label, span })
    }

    /// Parse continue statement: continue or continue label  
    fn parse_continue_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Continue, "expected 'continue'")?;

        let label = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            Some(name)
        } else {
            None
        };

        let span = self.span_from(start_pos);

        Ok(Stmt::Continue { label, span })
    }

    /// Parse defer statement: defer expr
    fn parse_defer_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::Defer, "expected 'defer'")?;

        let expr = self.parse_expression()?;
        let span = self.span_from(start_pos);

        Ok(Stmt::Defer {
            expr: Box::new(expr),
            span,
        })
    }

    /// Parse block statement: { statements... }
    fn parse_block_statement(&mut self, start_pos: Position) -> ParseResult<Stmt> {
        self.consume(&TokenType::LeftBrace, "expected '{'")?;

        let mut statements = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(Box::new(self.parse_statement()?));
            self.skip_trivia();
        }

        self.consume(&TokenType::RightBrace, "expected '}' after block")?;

        let span = self.span_from(start_pos);

        Ok(Stmt::Block { statements, span })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use bolt_ast::Expr;
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

    #[test]
    fn test_if_statement() {
        let mut parser = create_stmt_parser("if x > 0 : print(\"positive\") end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::If {
            condition,
            else_branch,
            ..
        } = stmt
        {
            assert!(matches!(*condition, Expr::Binary { .. }));
            assert!(else_branch.is_none());
        } else {
            panic!("Expected if statement");
        }
    }

    #[test]
    fn test_if_else_statement() {
        let mut parser = create_stmt_parser("if x > 0 : print(\"pos\") else : print(\"neg\") end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::If { else_branch, .. } = stmt {
            assert!(else_branch.is_some());
        } else {
            panic!("Expected if-else statement");
        }
    }

    #[test]
    fn test_loop_statement() {
        let mut parser = create_stmt_parser("loop : print(\"forever\") end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::Loop { kind, .. } = stmt {
            assert!(matches!(kind, LoopKind::Infinite));
        } else {
            panic!("Expected loop statement");
        }
    }

    #[test]
    fn test_while_loop() {
        let mut parser = create_stmt_parser("loop (x > 0) : x = x - 1 end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::Loop { kind, .. } = stmt {
            assert!(matches!(kind, LoopKind::While(_)));
        } else {
            panic!("Expected while loop");
        }
    }

    #[test]
    fn test_struct_definition() {
        let mut parser = create_stmt_parser("struct Point : x :: Int y :: Int end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::StructDef { name, fields, .. } = stmt {
            assert_eq!(name, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "x");
            assert_eq!(fields[1].name, "y");
        } else {
            panic!("Expected struct definition");
        }
    }

    #[test]
    fn test_enum_definition() {
        let mut parser = create_stmt_parser("enum Color : Red Green Blue end");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::EnumDef { name, variants, .. } = stmt {
            assert_eq!(name, "Color");
            assert_eq!(variants.len(), 3);
            assert_eq!(variants[0].name, "Red");
            assert_eq!(variants[1].name, "Green");
            assert_eq!(variants[2].name, "Blue");
        } else {
            panic!("Expected enum definition");
        }
    }

    #[test]
    fn test_break_continue() {
        let mut parser = create_stmt_parser("break");
        let stmt = parser.parse_statement().unwrap();
        assert!(matches!(stmt, Stmt::Break { label: None, .. }));

        let mut parser = create_stmt_parser("continue");
        let stmt = parser.parse_statement().unwrap();
        assert!(matches!(stmt, Stmt::Continue { label: None, .. }));
    }

    #[test]
    fn test_defer_statement() {
        let mut parser = create_stmt_parser("defer cleanup()");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::Defer { expr, .. } = stmt {
            assert!(matches!(*expr, Expr::Call { .. }));
        } else {
            panic!("Expected defer statement");
        }
    }

    #[test]
    fn test_import_statement() {
        let mut parser = create_stmt_parser("import Math");
        let stmt = parser.parse_statement().unwrap();

        if let Stmt::Import { path, alias, .. } = stmt {
            assert!(matches!(path, ImportPath::Simple(name) if name == "Math"));
            assert!(alias.is_none());
        } else {
            panic!("Expected import statement");
        }
    }
}
