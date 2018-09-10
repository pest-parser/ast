extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest_deconstruct;

mod parser {
    #[derive(Parser)]
    #[grammar = "../examples/enums.pest"]
    pub struct MyParser;
    const _GRAMMAR: &str = include_str!("enums.pest");
}

mod ast {
    use super::parser::Rule;
    use pest::Span;

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::ident")]
    pub struct Ident<'i> {
        span: Span<'i>,
    }

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::number")]
    pub struct Number<'i> {
        span: Span<'i>,
        #[pest(parse)]
        value: usize,
    }

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::term")]
    pub enum Term<'i> {
        Ident(Ident<'i>),
        Number(Number<'i>),
    }

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::terms")]
    pub struct Terms<'i> {
        span: Span<'i>,
        terms: Vec<Term<'i>>,
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use parser::{MyParser, Rule};
    use pest::Parser;
    use pest_deconstruct::FromPest;

    let parse = MyParser::parse(Rule::terms, "word 53 id34 88")?
        .next()
        .unwrap();
    println!("ppt: {:#?}", parse);
    let expr = ast::Terms::from_pest(parse);
    println!("ast: {:#?}", expr);
    Ok(())
}
