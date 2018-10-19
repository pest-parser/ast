use {
    proc_macro2::TokenStream,
    syn::{parse::Error, parse::Result, spanned::Spanned, Fields, Ident, Index, Member, Path},
};

use attributes::PestAstAttribute;

#[derive(Clone, Debug)]
enum ConversionStrategy {
    FromPest,
    Outer(Vec<Path>),
    Inner(Vec<Path>, Option<Path>),
}

impl ConversionStrategy {
    fn from_attrs(attrs: Vec<PestAstAttribute>) -> Result<Self> {
        attrs
            .into_iter()
            .fold(Ok(ConversionStrategy::FromPest), |strategy, attr| {
                match (strategy?, attr) {
                    (ConversionStrategy::Outer(_), PestAstAttribute::Outer(attr)) => Err(
                        Error::new(attr.span(), "duplicate `outer` attribute not allowed"),
                    ),
                    (ConversionStrategy::Inner(_, _), PestAstAttribute::Inner(attr)) => Err(
                        Error::new(attr.span(), "duplicate `inner` attribute not allowed"),
                    ),
                    (ConversionStrategy::Inner(_, Some(_)), PestAstAttribute::Rule(attr)) => Err(
                        Error::new(attr.span(), "duplicate `rule` attribute not allowed"),
                    ),
                    (ConversionStrategy::Outer(_), PestAstAttribute::Inner(attr)) => Err(
                        Error::new(attr.span(), "cannot specify both `inner` and `outer`"),
                    ),
                    (ConversionStrategy::Inner(_, _), PestAstAttribute::Outer(attr)) => Err(
                        Error::new(attr.span(), "cannot specify both `outer` and `inner`"),
                    ),

                    (ConversionStrategy::FromPest, PestAstAttribute::Outer(_)) => {
                        Ok(ConversionStrategy::Outer(vec![]))
                    }
                    (ConversionStrategy::FromPest, PestAstAttribute::Inner(_)) => {
                        Ok(ConversionStrategy::Inner(vec![], None))
                    }
                    (ConversionStrategy::FromPest, PestAstAttribute::With(attr)) => {
                        Err(Error::new(
                            attr.span(),
                            "attribute only allowed after `inner` or `outer`",
                        ))
                    }

                    (ConversionStrategy::Outer(mut with), PestAstAttribute::With(attr)) => {
                        with.push(attr.path);
                        Ok(ConversionStrategy::Outer(with))
                    }

                    (ConversionStrategy::Inner(mut with, rule), PestAstAttribute::With(attr)) => {
                        with.push(attr.path);
                        Ok(ConversionStrategy::Inner(with, rule))
                    }
                    (ConversionStrategy::Inner(with, None), PestAstAttribute::Rule(attr)) => {
                        Ok(ConversionStrategy::Inner(with, Some(attr.path)))
                    }

                    (_, PestAstAttribute::Grammar(attr)) => {
                        Err(Error::new(attr.span(), "attribute not allowed here"))
                    }
                    (_, PestAstAttribute::Rule(attr)) => Err(Error::new(
                        attr.span(),
                        "attribute only allowed after `inner`",
                    )),
                }
            })
    }

    fn apply(self, _name: &Ident, member: Member) -> TokenStream {
        let conversion = match self {
            ConversionStrategy::FromPest => quote!(::from_pest::FromPest::from_pest(inner)?),
            ConversionStrategy::Outer(mods) => with_mods(quote!(span.clone()), mods),
            ConversionStrategy::Inner(mods, rule) => {
                let pair = quote!(inner.next().ok_or(::from_pest::ConversionError::NoMatch)?);
                let span = if let Some(rule) = rule {
                    quote! {{
                        let pair = #pair;
                        if pair.as_rule() == #rule {
                            pair.as_span()
                        } else {
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
                    quote!(#pair.as_span())
                };
                with_mods(span, mods)
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
        .rev()
        .fold(stream, |stream, path| quote!(#path(#stream)))
}

pub fn convert(name: &Ident, fields: Fields) -> Result<TokenStream> {
    Ok(match fields {
        Fields::Named(fields) => {
            let fields: Vec<_> = fields
                .named
                .into_iter()
                .map(|field| {
                    let attrs = PestAstAttribute::from_attributes(field.attrs)?;
                    Ok(ConversionStrategy::from_attrs(attrs)?
                        .apply(name, Member::Named(field.ident.unwrap())))
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
                    let attrs = PestAstAttribute::from_attributes(field.attrs)?;
                    Ok(ConversionStrategy::from_attrs(attrs)?
                        .apply(name, Member::Unnamed(Index::from(i))))
                })
                .collect::<Result<_>>()?;
            quote!(#name(#(#fields),*))
        }
        Fields::Unit => quote!(#name),
    })
}
