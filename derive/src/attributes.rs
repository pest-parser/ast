use super::Result;
use itertools::Itertools;
use syn::{spanned::Spanned, Attribute, Lit, Meta, NestedMeta, Path};
use utils::{accumulate, adapt_error};

pub enum PestAttribute {
    Rule(Path),
    Parse,
    DiscardTrailing,
    Skip(Vec<Path>),
}

impl PestAttribute {
    fn try_from(meta: NestedMeta) -> Result<PestAttribute> {
        Ok(match &meta {
            NestedMeta::Literal(meta) => Err(("Unknown attribute".to_string(), meta.span()))?,
            NestedMeta::Meta(meta) => match meta {
                Meta::Word(meta) if meta == "parse" => PestAttribute::Parse,
                Meta::Word(meta) if meta == "discard_trailing" => PestAttribute::DiscardTrailing,
                Meta::NameValue(meta) if meta.ident == "rule" => match &meta.lit {
                    Lit::Str(lit) => PestAttribute::Rule(lit.parse().map_err(adapt_error)?),
                    lit => Err(("Expected string literal".to_string(), lit.span()))?,
                },
                Meta::List(meta) if meta.ident == "skip" => PestAttribute::Skip(
                    meta.nested
                        .iter()
                        .map(|meta| match meta {
                            NestedMeta::Literal(Lit::Str(lit)) => lit.parse().map_err(adapt_error),
                            meta => Err(("Expected string literal".to_string(), meta.span())),
                        }).fold_results(vec![], accumulate)?,
                ),
                meta => Err(("Unknown attribute".to_string(), meta.span()))?,
            },
        })
    }

    pub fn rule(&self) -> Option<&Path> {
        match self {
            PestAttribute::Rule(path) => Some(path),
            _ => None,
        }
    }

    pub fn parse(&self) -> bool {
        match self {
            PestAttribute::Parse => true,
            _ => false,
        }
    }

    pub fn discard_trailing(&self) -> bool {
        match self {
            PestAttribute::DiscardTrailing => true,
            _ => false,
        }
    }
}

pub fn pest_attributes<'it>(
    it: impl IntoIterator<Item = &'it Attribute> + 'it,
) -> Result<Vec<PestAttribute>> {
    it.into_iter()
        .flat_map(Attribute::interpret_meta)
        .flat_map(|meta| match meta {
            Meta::List(meta) => {
                if meta.ident == "pest" {
                    meta.nested.into_iter()
                } else {
                    ::syn::punctuated::Punctuated::new().into_iter()
                }
            }
            _ => ::syn::punctuated::Punctuated::new().into_iter(),
        }).map(PestAttribute::try_from)
        .fold_results(vec![], accumulate)
}
