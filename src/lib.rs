#[doc(hidden)]
pub extern crate pest;
extern crate void;

#[doc(inline)]
pub use void::Void;

use {
    pest::{
        iterators::{Pair, Pairs},
        RuleType,
    },
    std::marker::PhantomData,
};

/// An error that occurs during conversion.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConversionError<FatalError> {
    NoMatch,
    Malformed(FatalError),
}

/// Potentially borrowing conversion from a pest parse tree.
pub trait FromPest<'pest>: Sized {
    /// The rule type for the parse tree this type corresponds to.
    type Rule: RuleType;
    /// A fatal error during conversion.
    type FatalError;
    /// Convert from a Pest parse tree.
    ///
    /// # Return type semantics
    ///
    /// - `Err(ConversionError::NoMatch)` => node not at head of the cursor, cursor unchanged
    /// - `Err(ConversionError::Malformed)` => fatal error; node at head of the cursor but malformed
    /// - `Ok` => success; the cursor has been updated past this node
    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>>;
}

/// Convert a production without storing it.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for PhantomData<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<T::FatalError>> {
        T::from_pest(pest).map(|_| PhantomData)
    }
}

/// For recursive grammars.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Box<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<T::FatalError>> {
        T::from_pest(pest).map(Box::new)
    }
}

/// Convert an optional production.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Option<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<T::FatalError>> {
        match T::from_pest(pest) {
            Err(ConversionError::NoMatch) => Ok(None),
            result => result.map(Some),
        }
    }
}

/// Convert many productions. (If `<T as FromPest>` is non-advancing, this will be non-terminating.)
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Vec<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<T::FatalError>> {
        let mut acc = vec![];
        loop {
            match T::from_pest(pest) {
                Ok(t) => acc.push(t),
                Err(ConversionError::NoMatch) => break,
                Err(error @ ConversionError::Malformed(_)) => return Err(error),
            }
        }
        Ok(acc)
    }
}

/// Consume a production without doing any processing.
impl<'pest, Rule: RuleType> FromPest<'pest> for Pair<'pest, Rule> {
    type Rule = Rule;
    type FatalError = Void;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<Void>> {
        pest.next().ok_or(ConversionError::NoMatch)
    }
}
