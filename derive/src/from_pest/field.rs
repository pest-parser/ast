use {
    proc_macro2::{Span, TokenStream},
    syn::{
        parse::Error, parse::Result, parse_quote, spanned::Spanned, Fields, Index, Member, Path,
    },
};

use crate::attributes::FieldAttribute;
use crate::trace;

#[derive(Clone, Debug)]
enum ConversionStrategy {
    FromPest,
    Outer(Span, Vec<Path>),
    Inner(Span, Vec<Path>, Option<Path>),
}

impl ConversionStrategy {
    fn from_attrs(attrs: Vec<FieldAttribute>) -> Result<Self> {
        let mut attrs = attrs.into_iter();
        Ok(match (attrs.next(), attrs.next()) {
            (Some(_), Some(attr)) => Err(Error::new(
                attr.span(),
                "only a single field attribute allowed",
            ))?,
            (None, None) => ConversionStrategy::FromPest,
            (Some(FieldAttribute::Outer(attr)), None) => ConversionStrategy::Outer(
                attr.span(),
                attr.with.into_iter().map(|attr| attr.path).collect(),
            ),
            (Some(FieldAttribute::Inner(attr)), None) => ConversionStrategy::Inner(
                attr.span(),
                attr.with.into_iter().map(|attr| attr.path).collect(),
                attr.rule.map(|attr| {
                    let path = attr.path;
                    let variant = attr.variant;
                    parse_quote!(#path::#variant)
                }),
            ),
            _ => unreachable!(),
        })
    }

    fn apply(self, member: Member) -> TokenStream {
        let conversion = match self {
            ConversionStrategy::FromPest => quote!(::from_pest::FromPest::from_pest(inner)?),
            ConversionStrategy::Outer(span, mods) => with_mods(quote_spanned!(span=>span), mods),
            ConversionStrategy::Inner(span, mods, rule) => {
                let pair = quote!(inner.next().ok_or(::from_pest::ConversionError::NoMatch)?);
                let get_span = if let Some(rule) = rule {
                    let error_msg = trace(quote! {
                        concat!(
                            "in ",
                            stringify!(#member),
                            ", expected `",
                            stringify!(#rule),
                            "` but found `{:?}`"
                        ),
                        pair.as_rule(),
                    });
                    quote_spanned! {span=>{
                        let pair = #pair;
                        if pair.as_rule() == #rule {
                            pair.as_span()
                        } else {
                            #error_msg
                            return Err(::from_pest::ConversionError::NoMatch)
                            // TODO: Should this be panicking instead?
                            // panic!(
                            //     concat!(
                            //         "in ",
                            //         stringify!(#name),
                            //         ".",
                            //         stringify!(#member),
                            //         ", expected `",
                            //         stringify!(#rule),
                            //         "` but found `{:?}`"
                            //     ),
                            //     pair.as_rule(),
                            // )
                        }
                    }}
                } else {
                    quote_spanned!(span=>#pair.as_span())
                };
                with_mods(get_span, mods)
            }
        };
        if let Member::Named(name) = member {
            quote!(#name : #conversion)
        } else {
            conversion
        }
    }
}

fn with_mods(stream: TokenStream, mods: Vec<Path>) -> TokenStream {
    mods.into_iter()
        .fold(stream, |stream, path| quote!(#path(#stream)))
}

pub fn convert(name: &Path, fields: Fields) -> Result<TokenStream> {
    Ok(match fields {
        Fields::Named(fields) => {
            let fields: Vec<_> = fields
                .named
                .into_iter()
                .map(|field| {
                    let attrs = FieldAttribute::from_attributes(field.attrs)?;
                    Ok(ConversionStrategy::from_attrs(attrs)?
                        .apply(Member::Named(field.ident.unwrap())))
                })
                .collect::<Result<_>>()?;
            quote!(#name{#(#fields,)*})
        }
        Fields::Unnamed(fields) => {
            let fields: Vec<_> = fields
                .unnamed
                .into_iter()
                .enumerate()
                .map(|(i, field)| {
                    let attrs = FieldAttribute::from_attributes(field.attrs)?;
                    Ok(ConversionStrategy::from_attrs(attrs)?
                        .apply(Member::Unnamed(Index::from(i))))
                })
                .collect::<Result<_>>()?;
            quote!(#name(#(#fields),*))
        }
        Fields::Unit => quote!(#name),
    })
}
