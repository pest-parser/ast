#![allow(
    bad_style,
    dead_code,
    clippy::clone_on_copy,
    clippy::upper_case_acronyms
)]

#[macro_use]
extern crate pest_derive;
extern crate from_pest;
#[macro_use]
extern crate pest_ast;
extern crate pest;

use from_pest::FromPest;
use pest::Parser;

#[derive(Parser)]
#[grammar = "../examples/function_defaults.pest"]
pub struct FunctionParser;

// Define a simple enum for types that can have a default
#[derive(Debug, Clone, PartialEq)]
#[derive(Default)]
pub enum Type {
    #[default]
    Void,
    Int,
    String,
}


// Implement FromPest for Type
impl<'pest> FromPest<'pest> for Type {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let pair = pest.next().ok_or(from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == Rule::type_name {
            let span = pair.as_span();
            match span.as_str() {
                "void" => Ok(Type::Void),
                "int" => Ok(Type::Int),
                "string" => Ok(Type::String),
                _ => Err(from_pest::ConversionError::NoMatch),
            }
        } else {
            Err(from_pest::ConversionError::NoMatch)
        }
    }
}

// Define the AST types

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::id))]
pub struct Id<'pest> {
    #[pest_ast(outer())]
    pub span: pest::Span<'pest>,
}

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::param))]
pub struct Param<'pest> {
    pub id: Id<'pest>,
    pub type_name: Type,
}

// This is the key example: function return type with default
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::function))]
pub struct Function<'pest> {
    pub id: Id<'pest>,
    pub params: Vec<Param<'pest>>,

    // This demonstrates the new default feature!
    // Instead of Option<Type>, we use Type with a default
    #[pest_ast(default(Type::Void))]
    pub return_type: Type,
}

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::program))]
pub struct Program<'pest> {
    pub functions: Vec<Function<'pest>>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Test with a function that has no return type (should default)
    let input1 = "fn main() {}";
    let pairs1 = FunctionParser::parse(Rule::program, input1)?;
    let program1: Program = Program::from_pest(&mut pairs1.clone())?;
    println!("Program 1 (no return type): {program1:#?}");

    // Test with a function that has an explicit return type
    let input2 = "fn add() -> int {}";
    let pairs2 = FunctionParser::parse(Rule::program, input2)?;
    let program2: Program = Program::from_pest(&mut pairs2.clone())?;
    println!("Program 2 (explicit return type): {program2:#?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_with_default_return_type() {
        let input = "fn main() {}";
        let pairs = FunctionParser::parse(Rule::program, input).unwrap();
        let program: Program = Program::from_pest(&mut pairs.clone()).unwrap();

        assert_eq!(program.functions.len(), 1);
        let function = &program.functions[0];

        // The return type should be Void (the default) even though it wasn't specified
        assert_eq!(function.return_type, Type::Void);
        println!("Function return type: {:?}", function.return_type);
    }

    #[test]
    fn test_function_with_explicit_return_type() {
        let input = "fn add() -> int {}";
        let pairs = FunctionParser::parse(Rule::program, input).unwrap();
        let program: Program = Program::from_pest(&mut pairs.clone()).unwrap();

        assert_eq!(program.functions.len(), 1);
        let function = &program.functions[0];

        // The return type should be Int
        assert_eq!(function.return_type, Type::Int);
        println!("Function return type: {:?}", function.return_type);
    }
}
