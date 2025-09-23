//! Bolt Semantic Analysis
//!
//! This crate provides semantic analysis for the Bolt programming language,
//! including symbol resolution, type checking, and ownership analysis.

mod error;
mod resolver;
mod scope;
mod symbol;

pub use error::*;
pub use resolver::*;
pub use scope::*;
pub use symbol::*;

#[cfg(test)]
mod integration_tests {
    use super::*;
    use bolt_ast::Type;
    use bolt_lexer::Lexer;
    use bolt_parser::Parser;

    fn analyze_bolt_code(source: &str) -> Result<SemanticAnalyzer, SemanticErrors> {
        // Parse the code first
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Lexing should succeed");

        let mut parser = Parser::new(tokens);
        let statements = parser.parse_program().expect("Parsing should succeed");

        // Create program for semantic analysis
        let program = Program { statements };

        // Run semantic analysis
        let mut analyzer = SemanticAnalyzer::new();
        match analyzer.analyze(&program) {
            Ok(()) => Ok(analyzer),
            Err(errors) => {
                // Store the analyzer even if there are errors for inspection
                analyzer.errors = errors.clone();
                Err(errors)
            }
        }
    }

    #[test]
    fn test_simple_function_analysis() {
        let source = r#"
fn add(a :: Int, b :: Int) -> Int :
    a + b
end
"#;

        let analyzer = analyze_bolt_code(source).expect("Analysis should succeed");

        // Function should be in symbol table
        let symbol = analyzer.scope_manager().resolve_symbol("add").unwrap();
        assert_eq!(symbol.name, "add");

        if let SymbolKind::Function {
            params,
            return_type,
            ..
        } = &symbol.kind
        {
            assert_eq!(params.len(), 2);
            assert!(matches!(return_type, Type::Int));
        } else {
            panic!("Expected function symbol");
        }
    }

    #[test]
    fn test_variable_declaration_analysis() {
        let source = r#"
fn main() :
    let x = 42
    mut y :: String = "hello"
    return x
end
"#;

        let analyzer = analyze_bolt_code(source).expect("Analysis should succeed");

        // Function should be defined
        assert!(analyzer.scope_manager().resolve_symbol("main").is_some());

        // Variables should be properly scoped (not accessible from global scope)
        assert!(analyzer.scope_manager().resolve_symbol("x").is_none());
        assert!(analyzer.scope_manager().resolve_symbol("y").is_none());
    }

    #[test]
    fn test_undefined_variable_error() {
        let source = r#"
fn test() :
    return undefined_var
end
"#;

        let result = analyze_bolt_code(source);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors.has_errors());
        assert_eq!(errors.error_count(), 1);

        // Should be an undefined symbol error
        let error = &errors.errors()[0];
        assert!(
            matches!(error, SemanticError::UndefinedSymbol { name, .. } if name == "undefined_var")
        );
    }

    #[test]
    fn test_duplicate_function_error() {
        let source = r#"
fn duplicate() :
    return 1
end

fn duplicate() :
    return 2
end
"#;

        let result = analyze_bolt_code(source);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors.has_errors());
        assert_eq!(errors.error_count(), 1);

        // Should be a duplicate symbol error
        let error = &errors.errors()[0];
        assert!(
            matches!(error, SemanticError::DuplicateSymbol { name, .. } if name == "duplicate")
        );
    }

    #[test]
    fn test_class_and_method_analysis() {
        let source = r#"
class User :
    name :: String { get }
    
    init(name :: String)
    
    pub fn greet() :
        print("Hello")
    end
end
"#;

        let analyzer = analyze_bolt_code(source).expect("Analysis should succeed");

        // Class should be in symbol table
        let symbol = analyzer.scope_manager().resolve_symbol("User").unwrap();
        assert_eq!(symbol.name, "User");

        if let SymbolKind::Type { kind, .. } = &symbol.kind {
            assert!(matches!(kind, TypeKind::Class));
        } else {
            panic!("Expected type symbol");
        }
    }

    #[test]
    fn test_argument_count_mismatch() {
        let source = r#"
fn takes_two(a :: Int, b :: Int) -> Int :
    a + b
end

fn main() :
    return takes_two(42)  // Missing second argument
end
"#;

        let result = analyze_bolt_code(source);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors.has_errors());

        // Should be an argument count mismatch error
        let error = &errors.errors()[0];
        assert!(matches!(
            error,
            SemanticError::ArgumentCountMismatch {
                name,
                arg_count: 1,
                expected_count: 2,
                ..
            } if name == "takes_two"
        ));
    }

    #[test]
    fn test_immutable_assignment_error() {
        let source = r#"
fn main() :
    let x = 42
    x = 100  // Error: x is immutable
end
"#;

        let result = analyze_bolt_code(source);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors.has_errors());

        // Should be an assign to immutable error
        let error = &errors.errors()[0];
        assert!(matches!(error, SemanticError::AssignToImmutable { name, .. } if name == "x"));
    }

    #[test]
    fn test_mutable_assignment_success() {
        let source = r#"
fn main() :
    mut x = 42
    x = 100  // OK: x is mutable
end
"#;

        let analyzer = analyze_bolt_code(source).expect("Analysis should succeed");
        assert!(!analyzer.errors().has_errors());
    }

    #[test]
    fn test_scoping_rules() {
        let source = r#"
let global_var = 42

fn outer() :
    let outer_var = "outer"
    
    fn inner() :
        let inner_var = true
        return global_var  // Should resolve to global
    end
    
    return inner()  // Should resolve to inner function in this scope
end

fn main() :
    return outer()  // Use the outer function to avoid unused warning
end
"#;

        let result = analyze_bolt_code(source);

        // Should succeed, but might have some unused variable warnings
        match result {
            Ok(analyzer) => {
                // Global symbols should be accessible
                assert!(analyzer.scope_manager().resolve_symbol("outer").is_some());
                assert!(analyzer
                    .scope_manager()
                    .resolve_symbol("global_var")
                    .is_some());
                assert!(analyzer.scope_manager().resolve_symbol("main").is_some());

                // Nested function should NOT be accessible from global scope
                assert!(analyzer.scope_manager().resolve_symbol("inner").is_none());

                // Verify no serious errors (undefined symbols, etc.)
                let serious_errors: Vec<_> = analyzer
                    .errors()
                    .errors()
                    .iter()
                    .filter(|e| !e.is_warning())
                    .collect();

                if !serious_errors.is_empty() {
                    println!("Unexpected serious errors:");
                    for error in serious_errors {
                        println!("  - {}", error);
                    }
                    panic!("Expected no serious errors");
                }
            }
            Err(errors) => {
                println!("Analysis failed with errors:");
                for error in errors.errors() {
                    println!("  - {}", error);
                }
                panic!("Analysis should succeed");
            }
        }
    }
    #[test]
    fn test_impl_block_analysis() {
        let source = r#"
struct Point :
    x :: Float
    y :: Float
end

impl Point :
    pub fn new(x :: Float, y :: Float) -> Point :
        Point(x, y)
    end
    
    pub fn distance(&self, other :: Point) -> Float :
        42.0  // Simplified
    end
end
"#;

        let analyzer = analyze_bolt_code(source).expect("Analysis should succeed");

        // Struct should be defined
        assert!(analyzer.scope_manager().resolve_symbol("Point").is_some());

        // Should have analyzed impl block without errors
        assert!(!analyzer.errors().has_errors());
    }

    #[test]
    fn test_unused_variable_warning() {
        let source = r#"
fn main() :
    let used_var = 42
    let unused_var = 100  // Should generate warning
    return used_var
end
"#;

        let result = analyze_bolt_code(source);

        // Should succeed but have warnings
        match result {
            Ok(analyzer) => {
                assert!(analyzer.errors().has_warnings());
                assert!(!analyzer.errors().has_errors());

                let warnings = analyzer
                    .errors()
                    .errors_with_severity(crate::ErrorSeverity::Warning);
                assert!(!warnings.is_empty());

                let warning = warnings[0];
                assert!(
                    matches!(warning, SemanticError::UnusedVariable { name, .. } if name == "unused_var")
                );
            }
            Err(errors) => {
                // Check if we only have warnings
                assert!(!errors.has_errors());
                assert!(errors.has_warnings());
            }
        }
    }

    #[test]
    fn test_complex_program_analysis() {
        let source = r#"
enum Result :
    Ok
    Error
end

trait Drawable :
    fn draw() -> String
end

class Shape :
    pub color :: String { get }
    
    init(color :: String)
    
    pub fn show() :
        print("Showing shape")
    end
end

class Circle :: Shape :
    radius :: Float { get }
    
    init(radius :: Float, color :: String) :
        super(color)
        $radius = radius
    end
end

impl Drawable for Circle :
    fn draw() -> String :
        "Drawing circle"
    end
end

fn main() :
    let circle = Circle(5.0, "red")
    circle.show()
    let drawing = circle.draw()
    return 0
end
"#;

        let analyzer = analyze_bolt_code(source).expect("Analysis should succeed");

        // All major symbols should be defined
        assert!(analyzer.scope_manager().resolve_symbol("Result").is_some());
        assert!(analyzer
            .scope_manager()
            .resolve_symbol("Drawable")
            .is_some());
        assert!(analyzer.scope_manager().resolve_symbol("Shape").is_some());
        assert!(analyzer.scope_manager().resolve_symbol("Circle").is_some());
        assert!(analyzer.scope_manager().resolve_symbol("main").is_some());

        // Should have minimal errors
        if analyzer.errors().has_errors() {
            println!("Unexpected errors:");
            for error in analyzer.errors().errors() {
                println!("  - {}", error);
            }
            panic!("Expected no errors in complex program analysis");
        }
    }

    #[test]
    fn test_error_reporting_quality() {
        let source = r#"
fn broken() :
    unknown_function(1, 2, 3)
    undefined_var = 42
    immutable_var = 100
end
"#;

        let result = analyze_bolt_code(source);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors.error_count() >= 2); // Should catch multiple errors

        // Test error formatting
        let report = errors.format_report(source);
        assert!(report.contains("unknown_function"));
        assert!(report.contains("undefined_var"));

        println!("Error report:\n{}", report);
    }
}
