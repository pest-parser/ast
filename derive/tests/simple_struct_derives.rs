#![allow(bad_style)]

#[macro_use]
extern crate pest_derive;
extern crate from_pest;
#[macro_use]
extern crate pest_ast;
extern crate pest;

//use pest::Parser;

#[derive(Parser)]
#[grammar = "../tests/simple_struct_derives.pest"]
pub struct SimpleParser;

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::S))]
struct S<'pest> {
    #[pest_ast(outer)]
    span: pest::Span<'pest>,
    a: Vec<a<'pest>>,
    b: Vec<b<'pest>>,
    c: Vec<c<'pest>>,
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::a))]
struct a<'pest> {
    #[pest_ast(outer)]
    span: pest::Span<'pest>,
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::b))]
struct b<'pest> {
    #[pest_ast(outer)]
    span: pest::Span<'pest>,
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::c))]
struct c<'pest> {
    #[pest_ast(outer)]
    span: pest::Span<'pest>,
}

#[test]
fn main() -> Result<(), Box<dyn std::any::Any>> {
    Ok(())
}
