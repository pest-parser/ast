# pest-ast

Convert from [pest](https://pest.rs) parse trees into typed syntax trees with ease!

Which would you rather have?

<details><summary>Pest Parse Tree</summary>

```
[
    Pair {
        rule: file,
        span: Span {
            str: "65279,1179403647,1463895090\n3.1415927,2.7182817,1.618034\n-40,-273.15\n13,42\n65537\n",
            start: 0,
            end: 81
        },
        inner: [
            Pair {
                rule: record,
                span: Span {
                    str: "65279,1179403647,1463895090",
                    start: 0,
                    end: 27
                },
                inner: [
                    Pair {
                        rule: field,
                        span: Span {
                            str: "65279",
                            start: 0,
                            end: 5
                        },
                        inner: []
                    },
                    Pair {
                        rule: field,
                        span: Span {
                            str: "1179403647",
                            start: 6,
                            end: 16
                        },
                        inner: []
                    },
                    Pair {
                        rule: field,
                        span: Span {
                            str: "1463895090",
                            start: 17,
                            end: 27
                        },
                        inner: []
                    }
                ]
            },
            Pair {
                rule: record,
                span: Span {
                    str: "3.1415927,2.7182817,1.618034",
                    start: 28,
                    end: 56
                },
                inner: [
                    Pair {
                        rule: field,
                        span: Span {
                            str: "3.1415927",
                            start: 28,
                            end: 37
                        },
                        inner: []
                    },
                    Pair {
                        rule: field,
                        span: Span {
                            str: "2.7182817",
                            start: 38,
                            end: 47
                        },
                        inner: []
                    },
                    Pair {
                        rule: field,
                        span: Span {
                            str: "1.618034",
                            start: 48,
                            end: 56
                        },
                        inner: []
                    }
                ]
            },
            Pair {
                rule: record,
                span: Span {
                    str: "-40,-273.15",
                    start: 57,
                    end: 68
                },
                inner: [
                    Pair {
                        rule: field,
                        span: Span {
                            str: "-40",
                            start: 57,
                            end: 60
                        },
                        inner: []
                    },
                    Pair {
                        rule: field,
                        span: Span {
                            str: "-273.15",
                            start: 61,
                            end: 68
                        },
                        inner: []
                    }
                ]
            },
            Pair {
                rule: record,
                span: Span {
                    str: "13,42",
                    start: 69,
                    end: 74
                },
                inner: [
                    Pair {
                        rule: field,
                        span: Span {
                            str: "13",
                            start: 69,
                            end: 71
                        },
                        inner: []
                    },
                    Pair {
                        rule: field,
                        span: Span {
                            str: "42",
                            start: 72,
                            end: 74
                        },
                        inner: []
                    }
                ]
            },
            Pair {
                rule: record,
                span: Span {
                    str: "65537",
                    start: 75,
                    end: 80
                },
                inner: [
                    Pair {
                        rule: field,
                        span: Span {
                            str: "65537",
                            start: 75,
                            end: 80
                        },
                        inner: []
                    }
                ]
            },
            Pair {
                rule: EOI,
                span: Span {
                    str: "",
                    start: 81,
                    end: 81
                },
                inner: []
            }
        ]
    }
]
```
</details>
<details><summary>Typed Syntax Tree</summary>

```
File {
    records: [
        Record {
            fields: [
                Field {
                    value: 65279.0
                },
                Field {
                    value: 1179403647.0
                },
                Field {
                    value: 1463895090.0
                }
            ]
        },
        Record {
            fields: [
                Field {
                    value: 3.1415927
                },
                Field {
                    value: 2.7182817
                },
                Field {
                    value: 1.618034
                }
            ]
        },
        Record {
            fields: [
                Field {
                    value: -40.0
                },
                Field {
                    value: -273.15
                }
            ]
        },
        Record {
            fields: [
                Field {
                    value: 13.0
                },
                Field {
                    value: 42.0
                }
            ]
        },
        Record {
            fields: [
                Field {
                    value: 65537.0
                }
            ]
        }
    ],
    eoi: EOI
}
```
</details>

-----

The above parse tree is produced by the following pest grammar:

```pest
field = { (ASCII_DIGIT | "." | "-")+ }
record = { field ~ ("," ~ field)* }
file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
```

parsing this csv:

```csv
65279,1179403647,1463895090
3.1415927,2.7182817,1.618034
-40,-273.15
13,42
65537
```

And converting it to a typed syntax tree is as simple as the following code:

```rust
mod ast {
    use super::csv::Rule;
    use pest::Span;

    fn span_into_str(span: Span) -> &str {
        span.as_str()
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::field))]
    pub struct Field {
        #[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))]
        pub value: f64,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::record))]
    pub struct Record {
        pub fields: Vec<Field>,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::file))]
    pub struct File {
        pub records: Vec<Record>,
        eoi: EOI,
    }

    #[derive(Debug, FromPest)]
    #[pest_ast(rule(Rule::EOI))]
    struct EOI;
}
```

And doing the actual parse is as simple as

```rust
let mut parse_tree = csv::Parser::parse(csv::Rule::file, &source)?;
let syntax_tree = File::from_pest(&mut parse_tree).expect("infallible");
```
