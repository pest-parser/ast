//! Test for improved error messages.

#![allow(dead_code)]

#[macro_use]
extern crate pest_derive;
extern crate from_pest;
#[macro_use]
extern crate pest_ast;
extern crate pest;

mod grammar {
    #[derive(Parser)]
    #[grammar_inline = r#"
        value = { "number" | "string" }
        pair = { name ~ ":" ~ value }
        name = { ASCII_ALPHA+ }
        WHITESPACE = _{ " " }
    "#]
    pub struct Parser;
}

mod ast {
    use super::grammar::Rule;

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::name))]
    pub struct Name<'pest> {
        #[pest_ast(outer())]
        pub span: pest::Span<'pest>,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::value))]
    pub struct Value<'pest> {
        #[pest_ast(outer())]
        pub span: pest::Span<'pest>,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::pair))]
    pub struct Pair<'pest> {
        pub name: Name<'pest>,
        pub value: Value<'pest>,
    }
}

#[test]
fn test_no_match_error_message() {
    use from_pest::ConversionError;
    use from_pest::FromPest;
    use pest::Parser;

    // Parse "name: number" as a value rule (wrong rule type)
    let source = "name: number";
    let mut pairs = grammar::Parser::parse(grammar::Rule::pair, source).expect("parse success");

    // Try to parse as Value (should fail with informative error)
    let result: Result<ast::Value<'_>, _> = ast::Value::from_pest(&mut pairs);

    assert!(result.is_err());
    let error = result.unwrap_err();

    match &error {
        ConversionError::NoMatchWithInfo {
            current_node,
            expected,
            actual,
        } => {
            assert_eq!(*current_node, "Value");
            assert_eq!(*expected, "value");
            assert!(actual.contains("pair")); // The actual rule is 'pair'
        }
        _ => panic!("Expected NoMatchWithInfo error, got: {:?}", error),
    }

    // Verify the Display implementation
    let error_message = error.to_string();
    assert!(error_message.contains("Value"));
    assert!(error_message.contains("value"));
    assert!(error_message.contains("pair"));
}

#[test]
fn test_is_no_match_helper() {
    use from_pest::ConversionError;

    let no_match: ConversionError<()> = ConversionError::NoMatch;
    assert!(no_match.is_no_match());

    let no_match_with_info: ConversionError<()> = ConversionError::NoMatchWithInfo {
        current_node: "Test",
        expected: "expected",
        actual: "actual".to_string(),
    };
    assert!(no_match_with_info.is_no_match());

    let extraneous: ConversionError<()> = ConversionError::Extraneous {
        current_node: "Test",
        extraneous: "[extra]".to_string(),
    };
    assert!(!extraneous.is_no_match());
}

#[test]
fn test_extraneous_error_message() {
    use from_pest::ConversionError;
    use from_pest::Void;

    let error: ConversionError<Void> = ConversionError::Extraneous {
        current_node: "TestNode",
        extraneous: "[extra_rule1, extra_rule2]".to_string(),
    };

    let message = error.to_string();
    assert!(message.contains("TestNode"));
    assert!(message.contains("extra_rule1"));
    assert!(message.contains("extra_rule2"));
    assert!(message.contains("extraneous tokens"));
}
