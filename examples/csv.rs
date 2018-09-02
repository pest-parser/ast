extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest_deconstruct;

mod parser {
    #[derive(Parser)]
    #[grammar = "../examples/csv.pest"]
    pub struct CSVParser;
    const _GRAMMAR: &str = include_str!("csv.pest");
}

mod ast {
    use super::parser::Rule;
    use pest::Span;

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::field")]
    pub struct Field<'i> {
        pub span: Span<'i>,
        #[pest(parse)]
        pub value: f64,
    }

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::record")]
    pub struct Record<'i> {
        pub span: Span<'i>,
        pub fields: Vec<Field<'i>>,
    }

    #[derive(Debug, FromPest)]
    #[pest(rule = "Rule::file")]
    pub struct File<'i> {
        pub span: Span<'i>,
        pub records: Vec<Record<'i>>,
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use parser::{CSVParser, Rule};
    use pest_deconstruct::FromPest;
    use pest::Parser;
    use ast::File;

    let unparsed_file = String::from_utf8(std::fs::read("./examples/csv.csv")?)?;
    let parsed_file = CSVParser::parse(Rule::file, &unparsed_file).unwrap().next().unwrap();
    println!("ppt: {:#?}", parsed_file);
    let file = File::from_pest(parsed_file);
    println!("ast: {:#?}", file);
    println!();

    let mut field_sum: f64 = 0.0;
    let mut record_count: u64 = 0;

    for record in file.records {
        record_count += 1;
        for field in record.fields {
            field_sum += field.value;
        }
    }

    println!("Sum of fields: {}", field_sum);
    println!("Number of records: {}", record_count);

    Ok(())
}
