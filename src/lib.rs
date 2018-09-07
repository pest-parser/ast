//! # pest_deconstruct
//!
//! [`PestDeconstructor`] and [`FromPest`] are utilities to ease transforming the "dynamically"
//! typed Pest parse tree into a statically typed AST.
#![deny(missing_docs)]

extern crate pest;
#[allow(unused_imports)] // used via reexport
#[macro_use]
extern crate derive;

use pest::{
    iterators::{Pair, Pairs},
    RuleType,
};
use std::iter::Peekable;

pub use derive::*;

/// Convert from the Pest parse tree to a typed AST node.
pub trait FromPest<'a>: Sized {
    /// The rule enum that this AST pairs with.
    type Rule: RuleType;
    /// The specific rule that this AST represents.
    const RULE: Self::Rule;
    /// Convert from a Pest parse tree node to a new AST node.
    ///
    /// # Panics
    ///
    /// If `pest.as_rule() != <Self as FromPest>::RULE`.
    fn from_pest(pest: Pair<'a, Self::Rule>) -> Self;
}

/// Deconstruct a Pest `Pair` into its inner productions in a strongly-typed, panic-enforced manner.
/// See [`PestDeconstructor`] for more information.
pub trait PestDeconstruct<'i> {
    /// The rule enum that this pairs with.
    type Rule: RuleType;
    /// Create a new [`PestDeconstructor`] to disassemble the Pest parse tree.
    fn deconstruct(self) -> PestDeconstructor<'i, Self::Rule>;
}

impl<'i, R: RuleType> PestDeconstruct<'i> for Pair<'i, R> {
    type Rule = R;
    fn deconstruct(self) -> PestDeconstructor<'i, R> {
        PestDeconstructor {
            pairs: self.into_inner().peekable(),
        }
    }
}

/// Deconstruct a Pest `Pair` into its inner productions in a strongly-typed, panic-enforced manner.
///
/// Panics on `Drop` if not exhausted.
/// Use [`PestDeconstructor::discard`] if you mean to ignore productions.
#[derive(Clone, Debug)]
pub struct PestDeconstructor<'i, R: RuleType> {
    pairs: Peekable<Pairs<'i, R>>,
}

impl<'i, R: RuleType> Drop for PestDeconstructor<'i, R> {
    fn drop(&mut self) {
        assert_eq!(
            self.pairs.next(),
            None,
            "PestDeconstructor was not fully exhausted"
        )
    }
}

impl<'i, R: RuleType> PestDeconstructor<'i, R> {
    /// Get the next production from this parse tree node.
    ///
    /// Useful for `Node` productions.
    ///
    /// # Panics
    ///
    /// If there are no remaining productions or the next one is for the wrong rule.
    pub fn next<T: FromPest<'i, Rule = R>>(&mut self) -> T {
        T::from_pest(self.next_pair(T::RULE))
    }

    /// Get the next production from this parse tree node, if it is the correct rule.
    ///
    /// Useful for `Node?` productions.
    pub fn next_opt<T: FromPest<'i, Rule = R>>(&mut self) -> Option<T> {
        self.next_pair_opt(T::RULE).map(T::from_pest)
    }

    /// Get the next productions from this parse tree of a single rule type.
    /// If the next production is for a different rule, returns an empty vector.
    ///
    /// Useful for `Node*` productions.
    pub fn next_many<T: FromPest<'i, Rule = R>>(&mut self) -> Vec<T> {
        let mut children = vec![];
        while let Some(child) = self.next_opt() {
            children.push(child);
        }
        children
    }

    /// Get the next raw `Pair` of a certain rule type.
    ///
    /// # Panics
    ///
    /// If there are no remaining productions or the next one is for the wrong rule.
    pub fn next_pair(&mut self, rule: R) -> Pair<'i, R> {
        self.next_pair_opt(rule).unwrap_or_else(|| {
            panic!(
                "Expected {:?} child, got {:?}",
                rule,
                self.pairs.peek().map(Pair::as_rule)
            )
        })
    }

    /// Get the next raw `Pair` if it matches a certain rule.
    pub fn next_pair_opt(&mut self, rule: R) -> Option<Pair<'i, R>> {
        self.pairs.next().filter(|pair| pair.as_rule() == rule)
    }

    /// Get the next raw `Pair`s while they match a certain rule.
    pub fn next_pair_many(&mut self, rule: R) -> Vec<Pair<'i, R>> {
        let mut children = vec![];
        while let Some(child) = self.next_pair_opt(rule) {
            children.push(child);
        }
        children
    }

    /// Get the next production.
    ///
    /// This works without providing a concrete rule, unlike [`PestDeconstructor::next_pair`].
    ///
    /// # Panics
    ///
    /// If there are no more remaining productions to skip.
    pub fn next_untyped(&mut self) -> Pair<'i, R> {
        self.pairs
            .next()
            .unwrap_or_else(|| panic!("PestDeconstructor already exhausted at `skip`"))
    }

    /// Discard the remaining productions in this parse tree node.
    pub fn discard(self) {
        let _ = self.pairs;
    }
}
