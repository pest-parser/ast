#[macro_use]
extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest_deconstruct;

mod parser {
    #[derive(Parser)]
    #[grammar = "../examples/parse.pest"]
    pub struct Parser;
    const _GRAMMAR: &str = include_str!("parse.pest");
}

mod ast {
    use super::parser::Rule;

    #[derive(Debug)]
    struct Span<'i>(&'i str, usize, usize);
    impl<'i> From<::pest::Span<'i>> for Span<'i> {
        fn from(span: ::pest::Span<'i>) -> Self {
            Span(span.as_str(), span.start(), span.end())
        }
    }

    // Box
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    struct BoxInner<'i> {
        span: Span<'i>,
        inner: Box<BoxParseOuter<'i>>,
    }
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::number")]
    struct BoxParseOuter<'i> {
        span: Span<'i>,
        #[pest(parse)]
        value: Box<usize>,
    }
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    struct BoxParseInner<'i> {
        span: Span<'i>,
        #[pest(parse, rule = "Rule::number")]
        inner: Box<usize>,
    }

    // Vec
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::many_number")]
    struct VecInner<'i> {
        span: Span<'i>,
        inner: Vec<Inner<'i>>,
    }
    // VecParseOuter doesn't make sense
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::many_number")]
    struct VecParseInner<'i> {
        span: Span<'i>,
        #[pest(parse, rule = "Rule::number")]
        inner: Vec<usize>,
    }

    // Option
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    struct OptionInner<'i> {
        span: Span<'i>,
        inner: Option<OptionParseOuter<'i>>,
    }
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::number")]
    struct OptionParseOuter<'i> {
        span: Span<'i>,
        #[pest(parse)]
        value: Option<usize>,
    }
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    struct OptionParseInner<'i> {
        span: Span<'i>,
        #[pest(parse, rule = "Rule::number")]
        value: Option<usize>,
    }

    // Span
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::number")]
    struct SpanInner<'i> {
        span: Span<'i>,
    }
    // SpanParseOuter doesn't make sense
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    struct SpanParseInner<'i> {
        span: Span<'i>,
        #[pest(parse, rule = "Rule::number")]
        inner: Span<'i>,
    }

    // Other
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::number")]
    struct Inner<'i> {
        span: Span<'i>,
    }
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::number")]
    struct ParseOuter<'i> {
        span: Span<'i>,
        #[pest(parse)]
        value: usize,
    }
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    struct ParseInner<'i> {
        span: Span<'i>,
        #[pest(parse, rule = "Rule::number")]
        value: usize,
    }

    // Enums
    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::one_number")]
    enum Enum<'i> {
        Inner(Inner<'i>),
        #[pest(parse)]
        ParseOuter(usize),
        #[pest(parse, rule = "Rule::number")]
        ParseInner(usize),
        #[pest(rule = "Rule::number")]
        InnerSpan(Span<'i>),
    }
}

fn main() {
    use parser::{Parser, Rule};
}
