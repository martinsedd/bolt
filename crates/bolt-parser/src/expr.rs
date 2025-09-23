use crate::error::{ParseError, ParseResult};
use crate::parser::Parser;
use bolt_ast::{BinaryOp, Expr, LiteralValue, UnaryOp};
use bolt_lexer::{Position, Span, TokenType};

/// Operator precedence levels (higher number = higher precedence)
// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// enum Precedence {
//     None = 0,
//     Assignment = 1, // =, +=, -=, etc.
//     Or = 2,         // or
//     And = 3,        // and
//     Equality = 4,   // .eq(), .is() (method calls)
//     Comparison = 5, // <, >, <=, >=
//     Term = 6,       // +, -, ~= (concat)
//     Factor = 7,     // *, /, %
//     Unary = 8,      // -, not
//     Power = 9,      // ^
//     Call = 10,      // function calls, field access, indexing
//     Primary = 11,   // literals, identifiers, parentheses
// }

fn create_inferred_annotation(span: Span) -> bolt_ast::TypeAnnotation {
    bolt_ast::TypeAnnotation::inferred(span)
}

impl Parser {
    /// Parse an expression
    pub fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_assignment()
    }

    /// Parse assignment expressions: var = value, var += value
    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_or()?;

        if let Some(op) = self.match_assignment_operator() {
            let start_pos = expr.span().start;
            let value = self.parse_assignment()?; // Right associative
            let span = self.span_from(start_pos);
            let type_annotation = create_inferred_annotation(span);

            match op {
                TokenType::Equal => {
                    expr = Expr::Assignment {
                        target: Box::new(expr),
                        value: Box::new(value),
                        type_annotation,
                        span,
                    };
                }
                _ => {
                    // Convert compound assignment to binary + assignment
                    let binary_op = match op {
                        TokenType::PlusEqual => BinaryOp::Add,
                        TokenType::MinusEqual => BinaryOp::Subtract,
                        TokenType::StarEqual => BinaryOp::Multiply,
                        TokenType::SlashEqual => BinaryOp::Divide,
                        TokenType::TildeEqual => BinaryOp::Concat,
                        _ => unreachable!("Invalid assignment operator"),
                    };

                    let binary_expr = Expr::Binary {
                        left: Box::new(expr.clone()),
                        operator: binary_op,
                        right: Box::new(value),
                        type_annotation: create_inferred_annotation(span),
                        span,
                    };

                    expr = Expr::Assignment {
                        target: Box::new(expr),
                        value: Box::new(binary_expr),
                        type_annotation,
                        span,
                    };
                }
            }
        }

        Ok(expr)
    }

    /// Parse logical OR expressions: a or b or c
    fn parse_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_and()?;

        while self.match_token(&TokenType::Or) {
            let operator = BinaryOp::Or;
            let start_pos = expr.span().start;
            let right = self.parse_and()?;
            let span = self.span_from(start_pos);

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse logical AND expressions: a and b and c
    fn parse_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality()?;

        while self.match_token(&TokenType::And) {
            let operator = BinaryOp::And;
            let start_pos = expr.span().start;
            let right = self.parse_equality()?;
            let span = self.span_from(start_pos);

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse equality expressions (method calls): a.eq(b), a.is(b)
    fn parse_equality(&mut self) -> ParseResult<Expr> {
        self.parse_comparison()
    }

    /// Parse comparison expressions: <, >, <=, >=
    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;

        while let Some(op) = self.match_comparison_operator() {
            let start_pos = expr.span().start;
            let right = self.parse_term()?;
            let span = self.span_from(start_pos);

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse term expressions: +, -, ~= (concat)
    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;

        while let Some(op) = self.match_term_operator() {
            let start_pos = expr.span().start;
            let right = self.parse_factor()?;
            let span = self.span_from(start_pos);

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse factor expressions: *, /, %
    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;

        while let Some(op) = self.match_factor_operator() {
            let start_pos = expr.span().start;
            let right = self.parse_unary()?;
            let span = self.span_from(start_pos);

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse unary expressions: -, not
    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if let Some(op) = self.match_unary_operator() {
            let start_pos = self.current_position();
            let operand = self.parse_unary()?; // Right associative
            let span = self.span_from(start_pos);

            Ok(Expr::Unary {
                operator: op,
                operand: Box::new(operand),
                type_annotation: create_inferred_annotation(span),
                span,
            })
        } else {
            self.parse_power()
        }
    }

    /// Parse power expressions: ^
    fn parse_power(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_try()?;

        // Right associative: 2^3^4 = 2^(3^4)
        if self.match_token(&TokenType::Caret) {
            let start_pos = expr.span().start;
            let right = self.parse_power()?; // Right associative
            let span = self.span_from(start_pos);

            expr = Expr::Binary {
                left: Box::new(expr),
                operator: BinaryOp::Power,
                right: Box::new(right),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse try expressions: expr?
    fn parse_try(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_call()?;

        while self.match_token(&TokenType::Question) {
            let start_pos = expr.span().start;
            let span = self.span_from(start_pos);

            expr = Expr::Try {
                expr: Box::new(expr),
                type_annotation: create_inferred_annotation(span),
                span,
            };
        }

        Ok(expr)
    }

    /// Parse call expressions: func(args), obj.method(args), obj.field, obj[index]
    fn parse_call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            match &self.peek().token_type {
                TokenType::LeftParen => {
                    // Function call: func(args)
                    expr = self.finish_call(expr)?;
                }
                TokenType::Dot => {
                    // Method call or field access
                    expr = self.parse_dot_access(expr)?;
                }
                TokenType::LeftBracket => {
                    // Index access: obj[index]
                    expr = self.parse_index_access(expr)?;
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    /// Parse primary expressions: literals, identifiers, parentheses, etc.
    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let start_pos = self.current_position();

        match &self.peek().token_type.clone() {
            // Literals
            TokenType::Integer(n) => {
                let value = *n;
                self.advance();
                let span = self.span_from(start_pos);
                Ok(Expr::Literal {
                    value: LiteralValue::Integer(value),
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }
            TokenType::Float(n) => {
                let value = *n;
                self.advance();
                let span = self.span_from(start_pos);
                Ok(Expr::Literal {
                    value: LiteralValue::Float(value),
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }
            TokenType::String(s) => {
                let value = s.clone();
                self.advance();
                let span = self.span_from(start_pos);

                // Check if this is a string with interpolation
                if self.has_string_interpolation(&value) {
                    self.parse_string_interpolation(value, span)
                } else {
                    Ok(Expr::Literal {
                        value: LiteralValue::String(value),
                        type_annotation: create_inferred_annotation(span),
                        span,
                    })
                }
            }
            TokenType::Boolean(b) => {
                let value = *b;
                self.advance();
                let span = self.span_from(start_pos);
                Ok(Expr::Literal {
                    value: LiteralValue::Boolean(value),
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }

            // Identifiers
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance();
                let span = self.span_from(start_pos);
                Ok(Expr::Identifier {
                    name,
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }

            // Self reference
            TokenType::Dollar => {
                self.advance();
                let field = if self.check(&TokenType::Identifier("".to_string())) {
                    if let TokenType::Identifier(field_name) = &self.peek().token_type {
                        let field_name = field_name.clone();
                        self.advance();
                        Some(field_name)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let span = self.span_from(start_pos);
                Ok(Expr::SelfRef {
                    field,
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }

            // Parenthesized expressions or unit
            TokenType::LeftParen => {
                self.advance(); // consume '('

                if self.check(&TokenType::RightParen) {
                    // Unit literal: ()
                    self.advance(); // consume ')'
                    let span = self.span_from(start_pos);
                    Ok(Expr::Literal {
                        value: LiteralValue::Unit,
                        type_annotation: create_inferred_annotation(span),
                        span,
                    })
                } else {
                    let expr = self.parse_expression()?;
                    self.consume(&TokenType::RightParen, "expected ')' after expression")?;
                    Ok(expr)
                }
            }

            // Array literals: [1, 2, 3]
            TokenType::LeftBracket => {
                self.advance(); // consume '['
                let elements = if self.check(&TokenType::RightBracket) {
                    vec![]
                } else {
                    self.parse_comma_separated(|parser| parser.parse_expression())?
                };
                self.consume(
                    &TokenType::RightBracket,
                    "expected ']' after array elements",
                )?;
                let span = self.span_from(start_pos);
                Ok(Expr::Array {
                    elements,
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }

            // Map literals: {"key": value} or block expressions
            TokenType::LeftBrace => {
                // Look ahead to determine if this is a map or block
                if self.looks_like_map() {
                    self.parse_map_literal(start_pos)
                } else {
                    self.parse_block_expression(start_pos)
                }
            }

            // Super calls: super.method() or super(args)
            TokenType::Super => {
                self.advance();
                let span = self.span_from(start_pos);
                Ok(Expr::Identifier {
                    name: "super".to_string(),
                    type_annotation: create_inferred_annotation(span),
                    span,
                })
            }

            // Control flow expressions
            TokenType::If => self.parse_if_expression(start_pos),
            TokenType::Match => self.parse_match_expression(start_pos),
            TokenType::Loop => self.parse_loop_expression(start_pos),
            TokenType::Return => self.parse_return_expression(start_pos),

            // Lambda expressions: fn(params): body
            TokenType::Fn => self.parse_lambda_expression(start_pos),

            _ => Err(ParseError::expected_token("expression", self.peek())),
        }
    }

    // Helper methods for specific expression types

    /// Finish parsing a function call
    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        let start_pos = callee.span().start;
        self.advance(); // consume '('

        let args = if self.check(&TokenType::RightParen) {
            vec![]
        } else {
            self.parse_comma_separated(|parser| parser.parse_expression())?
        };

        self.consume(&TokenType::RightParen, "expected ')' after arguments")?;
        let span = self.span_from(start_pos);

        Ok(Expr::Call {
            callee: Box::new(callee),
            args,
            type_annotation: create_inferred_annotation(span),
            span,
        })
    }

    /// Parse dot access: obj.field or obj.method(args)
    fn parse_dot_access(&mut self, object: Expr) -> ParseResult<Expr> {
        let start_pos = object.span().start;
        self.advance(); // consume '.'

        let method_name = if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err(ParseError::expected_token(
                "method or field name",
                self.peek(),
            ));
        };

        if self.check(&TokenType::LeftParen) {
            // Method call: obj.method(args)
            self.advance(); // consume '('
            let args = if self.check(&TokenType::RightParen) {
                vec![]
            } else {
                self.parse_comma_separated(|parser| parser.parse_expression())?
            };
            self.consume(&TokenType::RightParen, "expected ')' after arguments")?;
            let span = self.span_from(start_pos);

            Ok(Expr::MethodCall {
                receiver: Box::new(object),
                method: method_name,
                args,
                type_annotation: create_inferred_annotation(span),
                span,
            })
        } else {
            // Field access: obj.field
            let span = self.span_from(start_pos);
            Ok(Expr::FieldAccess {
                object: Box::new(object),
                field: method_name,
                type_annotation: create_inferred_annotation(span),
                span,
            })
        }
    }

    /// Parse index access: obj[index]
    fn parse_index_access(&mut self, object: Expr) -> ParseResult<Expr> {
        let start_pos = object.span().start;
        self.advance(); // consume '['

        let index = self.parse_expression()?;
        self.consume(&TokenType::RightBracket, "expected ']' after index")?;
        let span = self.span_from(start_pos);

        Ok(Expr::Index {
            object: Box::new(object),
            index: Box::new(index),
            type_annotation: create_inferred_annotation(span),
            span,
        })
    }

    // Operator matching helpers

    fn match_assignment_operator(&mut self) -> Option<TokenType> {
        let assignment_ops = [
            TokenType::Equal,
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::StarEqual,
            TokenType::SlashEqual,
            TokenType::TildeEqual,
        ];
        self.match_any(&assignment_ops)
    }

    fn match_comparison_operator(&mut self) -> Option<BinaryOp> {
        match self.peek().token_type {
            TokenType::Less => {
                self.advance();
                Some(BinaryOp::Less)
            }
            TokenType::Greater => {
                self.advance();
                Some(BinaryOp::Greater)
            }
            // Note: <= and >= would need to be added to lexer as separate tokens
            _ => None,
        }
    }

    fn match_term_operator(&mut self) -> Option<BinaryOp> {
        match self.peek().token_type {
            TokenType::Plus => {
                self.advance();
                Some(BinaryOp::Add)
            }
            TokenType::Minus => {
                self.advance();
                Some(BinaryOp::Subtract)
            }
            // Note: ~= is handled as TildeEqual in assignment
            _ => None,
        }
    }

    fn match_factor_operator(&mut self) -> Option<BinaryOp> {
        match self.peek().token_type {
            TokenType::Star => {
                self.advance();
                Some(BinaryOp::Multiply)
            }
            TokenType::Slash => {
                self.advance();
                Some(BinaryOp::Divide)
            }
            TokenType::Percent => {
                self.advance();
                Some(BinaryOp::Modulo)
            }
            _ => None,
        }
    }

    fn match_unary_operator(&mut self) -> Option<UnaryOp> {
        match self.peek().token_type {
            TokenType::Minus => {
                self.advance();
                Some(UnaryOp::Minus)
            }
            TokenType::Not => {
                self.advance();
                Some(UnaryOp::Not)
            }
            _ => None,
        }
    }

    // Placeholder implementations for complex expression types
    // These will need full implementations

    fn has_string_interpolation(&self, _s: &str) -> bool {
        // TODO: Check if string contains {expr} patterns
        false
    }

    fn parse_string_interpolation(&mut self, _value: String, span: Span) -> ParseResult<Expr> {
        // TODO: Parse string interpolation
        Ok(Expr::Literal {
            value: LiteralValue::String(_value),
            type_annotation: create_inferred_annotation(span),
            span,
        })
    }

    fn looks_like_map(&mut self) -> bool {
        // TODO: Look ahead to see if this looks like {"key": value}
        false
    }

    fn parse_map_literal(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse map literals
        todo!("Map literal parsing")
    }

    fn parse_block_expression(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse block expressions
        todo!("Block expression parsing")
    }

    fn parse_if_expression(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse if expressions
        todo!("If expression parsing")
    }

    fn parse_match_expression(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse match expressions
        todo!("Match expression parsing")
    }

    fn parse_loop_expression(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse loop expressions
        todo!("Loop expression parsing")
    }

    fn parse_return_expression(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse return expressions
        todo!("Return expression parsing")
    }

    fn parse_lambda_expression(&mut self, _start_pos: Position) -> ParseResult<Expr> {
        // TODO: Parse lambda expressions
        todo!("Lambda expression parsing")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use bolt_lexer::Lexer;

    fn create_expr_parser(source: &str) -> Parser {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize test input");
        Parser::new(tokens)
    }

    #[test]
    fn test_literal_expressions() {
        let mut parser = create_expr_parser("42");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(
            expr,
            Expr::Literal {
                value: LiteralValue::Integer(42),
                ..
            }
        ));

        let mut parser = create_expr_parser("3.14");
        let expr = parser.parse_expression().unwrap();
        assert!(
            matches!(expr, Expr::Literal { value: LiteralValue::Float(f), .. } if (f - 3.14).abs() < f64::EPSILON)
        );

        let mut parser = create_expr_parser("true");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(
            expr,
            Expr::Literal {
                value: LiteralValue::Boolean(true),
                ..
            }
        ));

        let mut parser = create_expr_parser(r#""hello""#);
        let expr = parser.parse_expression().unwrap();
        assert!(
            matches!(expr, Expr::Literal { value: LiteralValue::String(ref s), .. } if s == "hello")
        );
    }

    #[test]
    fn test_identifier_expressions() {
        let mut parser = create_expr_parser("variable");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::Identifier { ref name, .. } if name == "variable"));
    }

    #[test]
    fn test_binary_expressions() {
        let mut parser = create_expr_parser("1 + 2");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(
            expr,
            Expr::Binary {
                operator: BinaryOp::Add,
                ..
            }
        ));

        let mut parser = create_expr_parser("a * b + c");
        let expr = parser.parse_expression().unwrap();
        // Should be parsed as (a * b) + c due to precedence
        assert!(matches!(
            expr,
            Expr::Binary {
                operator: BinaryOp::Add,
                ..
            }
        ));
    }

    #[test]
    fn test_unary_expressions() {
        let mut parser = create_expr_parser("-42");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(
            expr,
            Expr::Unary {
                operator: UnaryOp::Minus,
                ..
            }
        ));

        let mut parser = create_expr_parser("not true");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(
            expr,
            Expr::Unary {
                operator: UnaryOp::Not,
                ..
            }
        ));
    }

    #[test]
    fn test_function_call() {
        let mut parser = create_expr_parser("func()");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::Call { .. }));

        let mut parser = create_expr_parser("func(1, 2)");
        let expr = parser.parse_expression().unwrap();
        if let Expr::Call { args, .. } = expr {
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected function call");
        }
    }

    #[test]
    fn test_method_call() {
        let mut parser = create_expr_parser("obj.method()");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::MethodCall { .. }));
    }

    #[test]
    fn test_field_access() {
        let mut parser = create_expr_parser("obj.field");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::FieldAccess { .. }));
    }

    #[test]
    fn test_array_literal() {
        let mut parser = create_expr_parser("[1, 2, 3]");
        let expr = parser.parse_expression().unwrap();
        if let Expr::Array { elements, .. } = expr {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("Expected array literal");
        }
    }

    #[test]
    fn test_parenthesized_expressions() {
        let mut parser = create_expr_parser("(1 + 2) * 3");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(
            expr,
            Expr::Binary {
                operator: BinaryOp::Multiply,
                ..
            }
        ));
    }

    #[test]
    fn test_try_expressions() {
        let mut parser = create_expr_parser("func()?");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::Try { .. }));
    }

    #[test]
    fn test_assignment() {
        let mut parser = create_expr_parser("x = 42");
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::Assignment { .. }));
    }

    #[test]
    fn test_operator_precedence() {
        let mut parser = create_expr_parser("1 + 2 * 3");
        let expr = parser.parse_expression().unwrap();

        // Should be parsed as 1 + (2 * 3)
        if let Expr::Binary {
            operator: BinaryOp::Add,
            right,
            ..
        } = expr
        {
            assert!(matches!(
                *right,
                Expr::Binary {
                    operator: BinaryOp::Multiply,
                    ..
                }
            ));
        } else {
            panic!("Expected addition with multiplication on right");
        }
    }
}
