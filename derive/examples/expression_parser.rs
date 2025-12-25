//! Example demonstrating advanced pest_ast patterns:
//!
//! 1. **Repetition of anonymous sequences**: How to handle patterns like
//!    `comparison = { arith_expr ~ (comp_op ~ arith_expr)* }` where you have
//!    repeated pairs of (operator, operand) that don't have their own grammar rule.
//!
//! 2. **Nested choices**: How to handle patterns like
//!    `arith_expr = { term ~ ((plus|minus) ~ term)* }` where operators are
//!    defined as a choice between multiple alternatives.
//!
//! 3. **Parsing into enums**: How to use `FromPest` with enums to represent
//!    operator variants and expression types.
//!
//! ## Key Patterns Shown
//!
//! ### Pattern 1: Named Operator Rules -> Enum (with derive)
//! When you have `plus = { "+" }` and `minus = { "-" }` as separate rules,
//! you can derive `FromPest` for each and combine them in an enum that
//! also derives `FromPest`:
//! ```ignore
//! #[derive(FromPest)]
//! #[pest_ast(rule(Rule::plus))]
//! struct Plus;
//!
//! #[derive(FromPest)]
//! #[pest_ast(rule(Rule::minus))]
//! struct Minus;
//!
//! // The enum uses the parent rule that contains the choice
//! #[derive(FromPest)]
//! #[pest_ast(rule(Rule::arith_expr))]  // or the containing rule
//! enum AddOp { Plus(Plus), Minus(Minus) }
//! ```
//!
//! ### Pattern 2: Combined Operator Rule -> Enum (manual FromPest)
//! For `comp_op = { eq | neq | lt | gt }`, implement `FromPest` manually
//! to map the child rules to enum variants. This is useful when you want
//! a simpler enum without wrapper structs.
//!
//! ### Pattern 3: Repetition with Operator-Operand Pairs (manual FromPest)
//! For `term ~ ((plus|minus) ~ term)*`, the `(operator, operand)` pairs
//! don't have their own grammar rule, so you must implement `FromPest`
//! manually to consume pairs of tokens:
//! ```ignore
//! impl FromPest for ArithTail {
//!     fn from_pest(pest: &mut Pairs) -> Result<Self, ConversionError> {
//!         let op = AddOp::from_pest(pest)?;    // consume operator
//!         let term = Term::from_pest(pest)?;    // consume operand
//!         Ok(ArithTail { op, term })
//!     }
//! }
//! ```

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
#[grammar = "../examples/expression_parser.pest"]
pub struct ExprParser;

// =============================================================================
// AST Types
// =============================================================================

/// Helper to convert a Span to a string slice.
fn span_into_str(span: pest::Span<'_>) -> &str {
    span.as_str()
}

// -----------------------------------------------------------------------------
// Literals
// -----------------------------------------------------------------------------

/// A numeric literal like "42" or "123".
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::number))]
pub struct Number {
    /// The numeric value parsed from the input.
    #[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))]
    pub value: i64,
}

/// An identifier like "x" or "foo_bar".
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::identifier))]
pub struct Identifier<'pest> {
    /// The span containing the identifier text.
    #[pest_ast(outer())]
    pub span: pest::Span<'pest>,
}

impl<'pest> Identifier<'pest> {
    /// Get the identifier name as a string slice.
    pub fn name(&self) -> &str {
        self.span.as_str()
    }
}

// -----------------------------------------------------------------------------
// Atomic Expressions
// -----------------------------------------------------------------------------

/// An atomic expression: number, identifier, or parenthesized expression.
///
/// **Key Pattern**: Using an enum to represent grammar alternatives.
/// The enum derives `FromPest` with the parent rule, and each variant
/// wraps a type that can parse the alternative.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::atom))]
pub enum Atom<'pest> {
    /// A numeric literal.
    Number(Number),
    /// A variable/identifier reference.
    Identifier(Identifier<'pest>),
    /// A parenthesized expression (recursive).
    Parenthesized(Box<Expr<'pest>>),
}

// -----------------------------------------------------------------------------
// Multiplicative Operators: mul | div
// Pattern: Each operator has its own rule, combined into an enum.
// -----------------------------------------------------------------------------

/// The multiplication operator `*`.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::mul))]
pub struct Mul;

/// The division operator `/`.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::div))]
pub struct Div;

/// Multiplicative operators: `*` or `/`.
///
/// **Key Pattern**: Manual `FromPest` for operators that appear as children
/// of a parent rule. We check if the next token is `mul` or `div` and
/// consume it accordingly.
#[derive(Debug, Clone, PartialEq)]
pub enum MulOp {
    /// Multiplication.
    Mul(Mul),
    /// Division.
    Div(Div),
}

impl<'pest> FromPest<'pest> for MulOp {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        // Try to parse as Mul first
        if let Ok(mul) = Mul::from_pest(pest) {
            return Ok(MulOp::Mul(mul));
        }
        // Try Div
        if let Ok(div) = Div::from_pest(pest) {
            return Ok(MulOp::Div(div));
        }
        Err(from_pest::ConversionError::NoMatch)
    }
}

// -----------------------------------------------------------------------------
// Additive Operators: plus | minus
// Pattern: Each operator has its own rule, combined into an enum.
// -----------------------------------------------------------------------------

/// The addition operator `+`.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::plus))]
pub struct Plus;

/// The subtraction operator `-`.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::minus))]
pub struct Minus;

/// Additive operators: `+` or `-`.
///
/// **Key Pattern**: Manual `FromPest` to try each operator variant.
/// This is necessary because there's no grammar rule specifically for
/// "plus or minus" - they appear directly as children of `arith_expr`.
#[derive(Debug, Clone, PartialEq)]
pub enum AddOp {
    /// Addition.
    Plus(Plus),
    /// Subtraction.
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

// -----------------------------------------------------------------------------
// Comparison Operators: eq | neq | lt | gt
// Pattern: Combined rule with manual FromPest implementation.
// -----------------------------------------------------------------------------

/// Comparison operators.
///
/// **Key Pattern**: When you have a combined operator rule like
/// `comp_op = { eq | neq | lt | gt }`, you can implement `FromPest`
/// manually to inspect the child rule and map to enum variants.
/// This avoids needing wrapper structs for each operator.
#[derive(Debug, Clone, PartialEq)]
pub enum CompOp {
    /// Equality `==`.
    Eq,
    /// Inequality `!=`.
    Neq,
    /// Less than `<`.
    Lt,
    /// Greater than `>`.
    Gt,
}

impl<'pest> FromPest<'pest> for CompOp {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let pair = pest.next().ok_or(from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == Rule::comp_op {
            let inner = pair
                .into_inner()
                .next()
                .ok_or(from_pest::ConversionError::NoMatch)?;
            match inner.as_rule() {
                Rule::eq => Ok(CompOp::Eq),
                Rule::neq => Ok(CompOp::Neq),
                Rule::lt => Ok(CompOp::Lt),
                Rule::gt => Ok(CompOp::Gt),
                _ => Err(from_pest::ConversionError::NoMatch),
            }
        } else {
            Err(from_pest::ConversionError::NoMatch)
        }
    }
}

// -----------------------------------------------------------------------------
// Factor
// -----------------------------------------------------------------------------

/// A factor is an atomic expression.
/// In a more complete grammar, this might include unary operators.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::factor))]
pub struct Factor<'pest> {
    /// The atomic expression.
    pub atom: Atom<'pest>,
}

// -----------------------------------------------------------------------------
// Term: factor ~ ((mul | div) ~ factor)*
// Pattern: Manual FromPest for the (operator, operand) tail.
// -----------------------------------------------------------------------------

/// One `(operator, operand)` pair in a term's repetition.
///
/// **Key Pattern for Anonymous Sequences**:
/// For grammar `term = { factor ~ ((mul | div) ~ factor)* }`, the repeated
/// `(mul | div) ~ factor` pairs don't have their own rule name.
/// We implement `FromPest` manually to consume an operator followed by a factor.
#[derive(Debug, Clone, PartialEq)]
pub struct TermTail<'pest> {
    /// The multiplicative operator.
    pub op: MulOp,
    /// The right-hand operand.
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
        // Then get the factor - if operator succeeded, factor must succeed
        let factor = Factor::from_pest(pest).map_err(|_| from_pest::ConversionError::NoMatch)?;
        Ok(TermTail { op, factor })
    }
}

/// A term: `factor ~ ((mul | div) ~ factor)*`.
///
/// **Example**: `2 * 3 / 4` parses as:
/// - `first` = Factor(2)
/// - `rest` = [TermTail(Mul, Factor(3)), TermTail(Div, Factor(4))]
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::term))]
pub struct Term<'pest> {
    /// The first factor.
    pub first: Factor<'pest>,
    /// Additional (operator, factor) pairs.
    pub rest: Vec<TermTail<'pest>>,
}

// -----------------------------------------------------------------------------
// Arithmetic Expression: term ~ ((plus | minus) ~ term)*
// Pattern: Same as Term but with additive operators.
// -----------------------------------------------------------------------------

/// One `(operator, operand)` pair in an arithmetic expression.
///
/// **Key Pattern**: Manual `FromPest` to consume operator + operand pairs.
#[derive(Debug, Clone, PartialEq)]
pub struct ArithTail<'pest> {
    /// The additive operator.
    pub op: AddOp,
    /// The right-hand operand.
    pub term: Term<'pest>,
}

impl<'pest> FromPest<'pest> for ArithTail<'pest> {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let op = AddOp::from_pest(pest)?;
        let term = Term::from_pest(pest).map_err(|_| from_pest::ConversionError::NoMatch)?;
        Ok(ArithTail { op, term })
    }
}

/// An arithmetic expression: `term ~ ((plus | minus) ~ term)*`.
///
/// **Example**: `1 + 2 - 3` parses as:
/// - `first` = Term(1)
/// - `rest` = [ArithTail(Plus, Term(2)), ArithTail(Minus, Term(3))]
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::arith_expr))]
pub struct ArithExpr<'pest> {
    /// The first term.
    pub first: Term<'pest>,
    /// Additional (operator, term) pairs.
    pub rest: Vec<ArithTail<'pest>>,
}

// -----------------------------------------------------------------------------
// Comparison Expression: arith_expr ~ (comp_op ~ arith_expr)*
// Pattern: Using a combined operator rule.
// -----------------------------------------------------------------------------

/// One `(operator, operand)` pair in a comparison expression.
#[derive(Debug, Clone, PartialEq)]
pub struct CompTail<'pest> {
    /// The comparison operator.
    pub op: CompOp,
    /// The right-hand operand.
    pub expr: ArithExpr<'pest>,
}

impl<'pest> FromPest<'pest> for CompTail<'pest> {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Rule>,
    ) -> Result<Self, from_pest::ConversionError<from_pest::Void>> {
        let op = CompOp::from_pest(pest)?;
        let expr = ArithExpr::from_pest(pest).map_err(|_| from_pest::ConversionError::NoMatch)?;
        Ok(CompTail { op, expr })
    }
}

/// A comparison expression: `arith_expr ~ (comp_op ~ arith_expr)*`.
///
/// **Example**: `1 < 2 == 3` parses as:
/// - `first` = ArithExpr(1)
/// - `rest` = [CompTail(Lt, ArithExpr(2)), CompTail(Eq, ArithExpr(3))]
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::comparison))]
pub struct Comparison<'pest> {
    /// The first arithmetic expression.
    pub first: ArithExpr<'pest>,
    /// Additional (operator, expression) pairs.
    pub rest: Vec<CompTail<'pest>>,
}

// -----------------------------------------------------------------------------
// Top-level Expression
// -----------------------------------------------------------------------------

/// The top-level expression wrapper.
#[derive(FromPest, Debug, Clone, PartialEq)]
#[pest_ast(rule(Rule::expr))]
pub struct Expr<'pest> {
    /// The comparison expression.
    pub comparison: Comparison<'pest>,
}

// =============================================================================
// Example Usage
// =============================================================================

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Expression Parser Example ===\n");
    println!("This example demonstrates:\n");
    println!("1. Repetition of anonymous sequences (operator ~ operand)*");
    println!("2. Nested choices for operators (plus | minus)");
    println!("3. Parsing into enums\n");
    println!("-------------------------------------------\n");

    // Example 1: Simple arithmetic
    let input1 = "1 + 2 * 3";
    println!("Input: {input1:?}");
    let pairs = ExprParser::parse(Rule::expr, input1)?;
    let expr: Expr = Expr::from_pest(&mut pairs.clone())?;
    println!("Parsed AST: {expr:#?}");
    println!();

    // Example 2: Comparison with arithmetic
    let input2 = "x + 1 < y * 2";
    println!("Input: {input2:?}");
    let pairs = ExprParser::parse(Rule::expr, input2)?;
    let expr: Expr = Expr::from_pest(&mut pairs.clone())?;
    println!("Parsed AST: {expr:#?}");
    println!();

    // Example 3: Chained comparisons
    let input3 = "a == b != c";
    println!("Input: {input3:?}");
    let pairs = ExprParser::parse(Rule::expr, input3)?;
    let expr: Expr = Expr::from_pest(&mut pairs.clone())?;
    println!("Parsed AST: {expr:#?}");

    Ok(())
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_parsing() {
        let input = "42";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        // Should parse as: Expr -> Comparison -> ArithExpr -> Term -> Factor -> Atom::Number(42)
        if let Atom::Number(n) = &expr.comparison.first.first.first.atom {
            assert_eq!(n.value, 42);
        } else {
            panic!(
                "Expected Number, got {:?}",
                expr.comparison.first.first.first.atom
            );
        }
    }

    #[test]
    fn test_identifier_parsing() {
        let input = "foo";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        if let Atom::Identifier(id) = &expr.comparison.first.first.first.atom {
            assert_eq!(id.name(), "foo");
        } else {
            panic!("Expected Identifier");
        }
    }

    #[test]
    fn test_addition() {
        let input = "1 + 2";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        let arith = &expr.comparison.first;
        assert_eq!(arith.rest.len(), 1);
        assert!(matches!(arith.rest[0].op, AddOp::Plus(_)));
    }

    #[test]
    fn test_subtraction() {
        let input = "5 - 3";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        let arith = &expr.comparison.first;
        assert_eq!(arith.rest.len(), 1);
        assert!(matches!(arith.rest[0].op, AddOp::Minus(_)));
    }

    #[test]
    fn test_multiplication() {
        let input = "2 * 3";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        let term = &expr.comparison.first.first;
        assert_eq!(term.rest.len(), 1);
        assert!(matches!(term.rest[0].op, MulOp::Mul(_)));
    }

    #[test]
    fn test_division() {
        let input = "10 / 2";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        let term = &expr.comparison.first.first;
        assert_eq!(term.rest.len(), 1);
        assert!(matches!(term.rest[0].op, MulOp::Div(_)));
    }

    #[test]
    fn test_comparison_operators() {
        for (input, expected_op) in [
            ("1 == 2", CompOp::Eq),
            ("1 != 2", CompOp::Neq),
            ("1 < 2", CompOp::Lt),
            ("1 > 2", CompOp::Gt),
        ] {
            let pairs = ExprParser::parse(Rule::expr, input).unwrap();
            let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

            assert_eq!(expr.comparison.rest.len(), 1, "Input: {input}");
            assert_eq!(expr.comparison.rest[0].op, expected_op, "Input: {input}");
        }
    }

    #[test]
    fn test_chained_arithmetic() {
        // 1 + 2 - 3 + 4
        let input = "1 + 2 - 3 + 4";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        let arith = &expr.comparison.first;
        assert_eq!(arith.rest.len(), 3);
        assert!(matches!(arith.rest[0].op, AddOp::Plus(_)));
        assert!(matches!(arith.rest[1].op, AddOp::Minus(_)));
        assert!(matches!(arith.rest[2].op, AddOp::Plus(_)));
    }

    #[test]
    fn test_chained_comparisons() {
        // a < b == c != d
        let input = "a < b == c != d";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        assert_eq!(expr.comparison.rest.len(), 3);
        assert_eq!(expr.comparison.rest[0].op, CompOp::Lt);
        assert_eq!(expr.comparison.rest[1].op, CompOp::Eq);
        assert_eq!(expr.comparison.rest[2].op, CompOp::Neq);
    }

    #[test]
    fn test_operator_precedence_structure() {
        // 1 + 2 * 3 should parse with * binding tighter than +
        // Structure: ArithExpr { first: Term(1), rest: [(+, Term(2 * 3))] }
        let input = "1 + 2 * 3";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        // The first term should be just "1"
        assert_eq!(expr.comparison.first.first.rest.len(), 0);

        // The second term (after +) should be "2 * 3"
        assert_eq!(expr.comparison.first.rest.len(), 1);
        let second_term = &expr.comparison.first.rest[0].term;
        assert_eq!(second_term.rest.len(), 1);
        assert!(matches!(second_term.rest[0].op, MulOp::Mul(_)));
    }

    #[test]
    fn test_parenthesized_expression() {
        let input = "(1 + 2)";
        let pairs = ExprParser::parse(Rule::expr, input).unwrap();
        let expr: Expr = Expr::from_pest(&mut pairs.clone()).unwrap();

        // The atom should be Parenthesized containing an Expr
        assert!(matches!(
            expr.comparison.first.first.first.atom,
            Atom::Parenthesized(_)
        ));
    }
}
