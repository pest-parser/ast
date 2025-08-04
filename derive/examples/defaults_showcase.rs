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
#[grammar = "../examples/defaults_showcase.pest"]
pub struct ShowcaseParser;

// Define enum types that can have defaults
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    String,
    Bool,
    Void,
}

impl<'pest> FromPest<'pest> for Type {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let pair = pest.next().ok_or(from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == Rule::type_name {
            match pair.as_str() {
                "int" => Ok(Type::Int),
                "string" => Ok(Type::String),
                "bool" => Ok(Type::Bool),
                "void" => Ok(Type::Void),
                _ => Err(from_pest::ConversionError::NoMatch),
            }
        } else {
            Err(from_pest::ConversionError::NoMatch)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i32),
    String(String),
    Id(String),
}

impl<'pest> FromPest<'pest> for Expr {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let pair = pest.next().ok_or(from_pest::ConversionError::NoMatch)?;
        match pair.as_rule() {
            Rule::expr => {
                // The expr rule contains nested rules, so we need to look at its inner content
                let mut inner = pair.into_inner();
                let inner_pair = inner.next().ok_or(from_pest::ConversionError::NoMatch)?;
                match inner_pair.as_rule() {
                    Rule::number => Ok(Expr::Number(inner_pair.as_str().parse().unwrap())),
                    Rule::string => {
                        let s = inner_pair.as_str();
                        Ok(Expr::String(s[1..s.len() - 1].to_string())) // Remove quotes
                    }
                    Rule::id => Ok(Expr::Id(inner_pair.as_str().to_string())),
                    _ => Err(from_pest::ConversionError::NoMatch),
                }
            }
            Rule::number => Ok(Expr::Number(pair.as_str().parse().unwrap())),
            Rule::string => {
                let s = pair.as_str();
                Ok(Expr::String(s[1..s.len() - 1].to_string())) // Remove quotes
            }
            Rule::id => Ok(Expr::Id(pair.as_str().to_string())),
            _ => Err(from_pest::ConversionError::NoMatch),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarKind {
    Let,
    Const,
}

impl<'pest> FromPest<'pest> for VarKind {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let pair = pest.next().ok_or(from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == Rule::var_kind {
            match pair.as_str() {
                "let" => Ok(VarKind::Let),
                "const" => Ok(VarKind::Const),
                _ => Err(from_pest::ConversionError::NoMatch),
            }
        } else {
            Err(from_pest::ConversionError::NoMatch)
        }
    }
}

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::id))]
pub struct Id<'pest> {
    #[pest_ast(outer())]
    pub span: pest::Span<'pest>,
}

impl<'pest> Id<'pest> {
    pub fn name(&self) -> &str {
        self.span.as_str()
    }
}

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::type_annotation))]
pub struct TypeAnnotation {
    pub type_name: Type,
}

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::initializer))]
pub struct Initializer {
    pub expr: Expr,
}

// Variable declaration showcasing multiple default values
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::var_decl))]
pub struct VarDecl<'pest> {
    // Now this should parse correctly from the var_kind rule
    #[pest_ast(default(VarKind::Let))]
    pub kind: VarKind,

    pub id: Id<'pest>,

    // Type annotation defaults to 'void' if not specified
    #[pest_ast(default(TypeAnnotation { type_name: Type::Void }))]
    pub type_annotation: TypeAnnotation,

    // Initialization defaults to a placeholder value
    #[pest_ast(default(Initializer { expr: Expr::Number(0) }))]
    pub initializer: Initializer,
}

#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::program))]
pub struct Program<'pest> {
    pub declarations: Vec<VarDecl<'pest>>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Default Values Showcase ===\n");

    // Test 1: Minimal declaration (all defaults)
    let input1 = "let x;";
    println!("Input 1: {input1}");
    let pairs1 = ShowcaseParser::parse(Rule::program, input1)?;
    let program1: Program = Program::from_pest(&mut pairs1.clone())?;
    println!("Parsed: {program1:#?}\n");

    // Test 2: With type annotation
    let input2 = "let y: int;";
    println!("Input 2: {input2}");
    let pairs2 = ShowcaseParser::parse(Rule::program, input2)?;
    let program2: Program = Program::from_pest(&mut pairs2.clone())?;
    println!("Parsed: {program2:#?}\n");

    // Test 3: With initialization
    let input3 = "let z = 42;";
    println!("Input 3: {input3}");
    let pairs3 = ShowcaseParser::parse(Rule::program, input3)?;
    let program3: Program = Program::from_pest(&mut pairs3.clone())?;
    println!("Parsed: {program3:#?}\n");

    // Test 4: Fully specified
    let input4 = "const w: string = \"hello\";";
    println!("Input 4: {input4}");
    let pairs4 = ShowcaseParser::parse(Rule::program, input4)?;
    let program4: Program = Program::from_pest(&mut pairs4.clone())?;
    println!("Parsed: {program4:#?}\n");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_defaults_applied() {
        let input = "let x;";
        let pairs = ShowcaseParser::parse(Rule::program, input).unwrap();
        let program: Program = Program::from_pest(&mut pairs.clone()).unwrap();

        assert_eq!(program.declarations.len(), 1);
        let decl = &program.declarations[0];

        // All defaults should be applied
        assert_eq!(decl.kind, VarKind::Let);
        assert_eq!(decl.type_annotation.type_name, Type::Void);
        assert_eq!(decl.initializer.expr, Expr::Number(0));
        assert_eq!(decl.id.name(), "x");
    }

    #[test]
    fn test_explicit_values_override_defaults() {
        let input = "const y: int = 42;";
        let pairs = ShowcaseParser::parse(Rule::program, input).unwrap();
        let program: Program = Program::from_pest(&mut pairs.clone()).unwrap();

        assert_eq!(program.declarations.len(), 1);
        let decl = &program.declarations[0];

        // Explicit values should override defaults
        assert_eq!(decl.kind, VarKind::Const); // Now should be correctly parsed
        assert_eq!(decl.type_annotation.type_name, Type::Int); // Explicit
        assert_eq!(decl.initializer.expr, Expr::Number(42)); // Explicit
        assert_eq!(decl.id.name(), "y");
    }
}
