#![feature(try_trait)]

#[macro_use]
extern crate pest_ast;
extern crate from_pest;

mod pest {}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum Rule {
    A,
}

#[derive(FromPest)]
#[pest_ast(rule(Rule::A))]
struct A {
}
