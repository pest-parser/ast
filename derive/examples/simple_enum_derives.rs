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
#[grammar = "../examples/simple_enum_derives.pest"]
pub struct SimpleParser;

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::a))]
struct a<'pest> {
    #[pest_ast(outer())]
    span: pest::Span<'pest>,
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::b))]
struct b<'pest> {
    #[pest_ast(outer())]
    span: pest::Span<'pest>,
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::c))]
struct c<'pest> {
    #[pest_ast(outer())]
    span: pest::Span<'pest>,
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::abc))]
enum abc<'pest> {
    a(a<'pest>),
    b(b<'pest>),
    c(c<'pest>),
}

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::ABC))]
struct ABC<'pest> {
    abc: Vec<abc<'pest>>,
}

fn main() {
    let source = "aaabbbccc";

    let mut parse_tree = SimpleParser::parse(Rule::ABC, source).expect("parse success");
    println!("parse tree = {:#?}", parse_tree);

    let syntax_tree = ABC::from_pest(&mut parse_tree).expect("infallible");
    println!("syntax tree = {:#?}", syntax_tree);
}
