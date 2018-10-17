#[doc(hidden)]
pub extern crate pest;
#[doc(hidden)]
pub extern crate void;

use {
    pest::{
        iterators::{Pair, Pairs},
        RuleType,
    },
    std::{error::Error, fmt, marker::PhantomData},
    void::Void,
};

/// Potentially borrowing conversion from a pest parse tree.
pub trait FromPest<'pest>: Sized {
    /// The rule type for the parse tree this type corresponds to.
    type Rule: RuleType;
    /// An error type during conversion.
    type Error;
    /// Convert from a Pest parse tree.
    ///
    /// If returning `Ok(_)`, `pest` has been updated to consume the converted `Pair`(s).
    /// If returning `Err(_)`, `pest` has not been changed.
    fn from_pest(pest: &mut Pairs<'pest, Self::Rule>) -> Result<Self, Self::Error>;
}

/// Convert a production without storing it.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for PhantomData<T> {
    type Rule = Rule;
    type Error = T::Error;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, Self::Error> {
        let _ = T::from_pest(pest)?;
        Ok(std::marker::PhantomData)
    }
}

/// For recursive grammars.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Box<T> {
    type Rule = Rule;
    type Error = T::Error;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, Self::Error> {
        Ok(T::from_pest(pest)?.into())
    }
}

/// Convert an optional production.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Option<T> {
    type Rule = Rule;
    type Error = Void;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, Void> {
        let mut clone = pest.clone();
        Ok(T::from_pest(&mut clone).ok().map(|this| {
            *pest = clone;
            this
        }))
    }
}

/// Convert many productions. (If `<T as FromPest>::Error ~= Void`, this will be non-terminating.)
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Vec<T> {
    type Rule = Rule;
    type Error = Void;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, Void> {
        let mut acc = vec![];
        while let Ok(this) = T::from_pest(pest) {
            acc.push(this);
        }
        Ok(acc)
    }
}

/// Consume a production without doing any processing.
impl<'pest, Rule: RuleType> FromPest<'pest> for Pair<'pest, Rule> {
    type Rule = Rule;
    type Error = NoRemainingPairs;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, NoRemainingPairs> {
        pest.next().ok_or_else(Default::default)
    }
}

#[macro_export]
#[allow(non_snake_case)]
macro_rules! impl_FromPest_for_concrete {
    ($Rule:ty) => {
        impl<'pest> $crate::FromPest<'pest> for $crate::pest::Span<'pest> {
            type Rule = $Rule;
            type Error = $crate::NoRemainingPairs;
            fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, NoRemainingPairs> {
                Pair::from_pest(pest).map($crate::pest::iterators::Pair::into_span)
            }
        }

        impl<'pest> $crate::FromPest<'pest> for () {
            type Rule = $Rule;
            type Error = $crate::void::Void;
            fn from_pest(_: &mut Pairs<'pest, Rule>) -> Result<Self, $crate::void::Void> {
                Ok(())
            }
        }
    };
}

/// Error during conversion: no pairs remained to convert.
#[derive(Debug, Default)]
pub struct NoRemainingPairs {
    non_exhaustive: (),
}

impl Error for NoRemainingPairs {}
impl fmt::Display for NoRemainingPairs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expected any rule; ran out of input")
    }
}

mod hidden {
    pub trait Sealed {}
}

/// `From` : `Into` :: `FromPest` : `PestInto`. Gives you a `.convert()` on `Pairs`.
///
/// TODO: This name is horrible _do not keep it_!
pub trait PestInto<'pest, Rule: RuleType>: hidden::Sealed {
    /// Convert the head of this pest parse tree into a `FromPest` type.
    fn convert<T: FromPest<'pest, Rule = Rule>>(&mut self) -> Result<T, T::Error>;
}

impl<'pest, Rule: RuleType> hidden::Sealed for Pairs<'pest, Rule> {}
impl<'pest, Rule: RuleType> PestInto<'pest, Rule> for Pairs<'pest, Rule> {
    fn convert<T: FromPest<'pest, Rule = Rule>>(&mut self) -> Result<T, T::Error> {
        T::from_pest(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
    pub enum Rule {}
    impl_FromPest_for_concrete!(Rule);
}
