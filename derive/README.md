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

## Default Values for Optional Rules

A powerful feature for handling optional grammar rules without requiring `Option<T>` in your AST is the `#[pest_ast(default(...))]` attribute. This allows you to specify default values that will be used when optional rules are not present in the input.

### The Problem

When using optional rules in Pest grammar, you typically need `Option<T>` in your AST:

```rust
// Grammar: function = { "fn" ~ id ~ ("->" ~ type)? ~ "{" ~ "}" }

#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::function))]
pub struct Function {
    pub name: String,
    pub return_type: Option<Type>, // Optional field
}
```

### The Solution

With the `default` attribute, you can eliminate `Option<T>` and specify a default value:

```rust
#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::function))]
pub struct Function {
    pub name: String,
    
    #[pest_ast(default(Type::Void))] // Specify default value
    pub return_type: Type,           // No Option<T> needed!
}
```

### Usage Examples

```rust
// Simple defaults
#[pest_ast(default(Type::Void))]
pub return_type: Type,

// Complex defaults with expressions
#[pest_ast(default(Vec::new()))]
pub parameters: Vec<Parameter>,

#[pest_ast(default({
    Config {
        debug: false,
        optimization_level: 2,
    }
}))]
pub config: Config,
```

### How It Works

The `default` attribute generates code that:
1. First tries to parse the field normally using `FromPest`
2. If conversion fails with `NoMatch` (optional rule not present), uses the default value
3. If parsing fails with other errors, propagates the error

This provides a clean, type-safe way to handle optional grammar elements while keeping your AST representation simple and avoiding the complexity of `Option<T>` handling.

## Advanced Patterns: Enums, Nested Choices, and Repetitions

For more complex grammars involving operators and expression parsing, pest-ast provides patterns for handling:

1. **Parsing into enums** - representing grammar alternatives
2. **Nested choices** - patterns like `(plus | minus)`
3. **Repetitions of anonymous sequences** - patterns like `(operator ~ operand)*`

### Parsing into Enums

When your grammar has alternatives (using `|`), you can use enums to represent them.

**Example Grammar:**
```pest
abc = { a | b | c }
a = { "a" }
b = { "b" }
c = { "c" }
```

**Rust AST:**
```rust
#[derive(FromPest)]
#[pest_ast(rule(Rule::a))]
struct A;

#[derive(FromPest)]
#[pest_ast(rule(Rule::b))]
struct B;

#[derive(FromPest)]
#[pest_ast(rule(Rule::c))]
struct C;

// Enum uses the parent rule that contains the choice
#[derive(FromPest)]
#[pest_ast(rule(Rule::abc))]
enum Abc {
    A(A),
    B(B),
    C(C),
}
```

See `derive/examples/simple_enum_derives.rs` for a complete example.

### Nested Choices with Manual FromPest

For patterns like `arith_expr = { term ~ ((plus | minus) ~ term)* }`, the `(plus | minus)` choice
doesn't have its own grammar rule. You can handle this by implementing `FromPest` manually:

**Example Grammar:**
```pest
plus = { "+" }
minus = { "-" }
arith_expr = { term ~ ((plus | minus) ~ term)* }
```

**Rust AST:**
```rust
#[derive(FromPest)]
#[pest_ast(rule(Rule::plus))]
pub struct Plus;

#[derive(FromPest)]
#[pest_ast(rule(Rule::minus))]
pub struct Minus;

// Manual implementation tries each alternative
#[derive(Debug, Clone, PartialEq)]
pub enum AddOp {
    Plus(Plus),
    Minus(Minus),
}

impl<'pest> FromPest<'pest> for AddOp {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        // Try Plus first
        if let Ok(plus) = Plus::from_pest(pest) {
            return Ok(AddOp::Plus(plus));
        }
        // Try Minus
        if let Ok(minus) = Minus::from_pest(pest) {
            return Ok(AddOp::Minus(minus));
        }
        Err(from_pest::ConversionError::NoMatch)
    }
}
```

### Repetitions of Anonymous Sequences

For grammar patterns like `term ~ ((operator ~ operand)*)`, the repeated `(operator ~ operand)` pairs
don't have their own grammar rule. Create a "tail" struct with manual `FromPest` implementation:

**Example Grammar:**
```pest
term = { factor ~ ((mul | div) ~ factor)* }
```

**Rust AST:**
```rust
// One (operator, operand) pair from the repetition
#[derive(Debug)]
pub struct TermTail<'pest> {
    pub op: MulOp,
    pub factor: Factor<'pest>,
}

impl<'pest> FromPest<'pest> for TermTail<'pest> {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        // First try to get an operator - if not present, no match
        let op = MulOp::from_pest(pest)?;
        // Then get the operand
        let factor = Factor::from_pest(pest)
            .map_err(|_| from_pest::ConversionError::NoMatch)?;
        Ok(TermTail { op, factor })
    }
}

// The main structure uses Vec<TermTail> for the repetition
#[derive(FromPest, Debug)]
#[pest_ast(rule(Rule::term))]
pub struct Term<'pest> {
    pub first: Factor<'pest>,
    pub rest: Vec<TermTail<'pest>>,  // Handles the (op ~ operand)* part
}
```

For a complete working example demonstrating all these patterns with an expression parser,
see `derive/examples/expression_parser.rs` and `derive/examples/expression_parser.pest`.
