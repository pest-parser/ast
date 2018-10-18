use {
    itertools::Itertools,
    proc_macro2::{Span, TokenStream},
    quote::ToTokens,
    std::path::PathBuf as FilePath,
    syn::{
        parse::Error, parse::Result, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput,
        Field, Fields, Ident, Path,
    },
};

use attributes::PestAstAttribute;

fn top_level_attributes(attrs: Vec<PestAstAttribute>) -> Result<(Option<FilePath>, Path, Ident)> {
    if let (grammar, Some(rule_enum), Some(rule_variant)) = attrs
        .into_iter()
        .map(|attr| match attr {
            PestAstAttribute::Grammar(attr) => Ok((Some(attr), None, None)),
            PestAstAttribute::Rule(attr) => Ok((None, Some(attr.path), Some(attr.variant))),
            attr => Err(Error::new(attr.span(), "this attr not allowed here")),
        })
        .fold_results(Ok((None, None, None)), |acc: Result<_>, next| {
            let acc = acc?;
            Ok(match (acc, next) {
                ((a, None, None), (None, b, c)) => (a, b, c),
                ((None, b, c), (a, None, None)) => (a, b, c),
                ((Some(_), _, _), (Some(a), _, _)) => Err(Error::new(
                    a.span(),
                    "duplicate grammar specification not allowed",
                ))?,
                ((_, Some(_), _), (_, Some(b), _)) => Err(Error::new(
                    b.span(),
                    "duplicate rule enum specification not allowed",
                ))?,
                (_, (Some(_), Some(_), Some(_)))
                | (_, (None, None, None))
                | (_, (_, Some(_), None))
                | (_, (_, None, Some(_)))
                | ((_, None, Some(_)), _) => unreachable!(),
            })
        })?? {
        Ok((
            grammar.map(|grammar| {
                let s = grammar.into_token_stream().to_string();
                FilePath::from(s[1..s.len() - 1].to_string())
            }),
            rule_enum,
            rule_variant,
        ))
    } else {
        Err(Error::new(
            Span::call_site(),
            "Exactly one `#[pest_ast(rule(path::to))]` required",
        ))
    }
}

pub(crate) fn derive(
    DeriveInput {
        attrs,
        ident: name,
        generics,
        data,
        ..
    }: DeriveInput,
) -> Result<TokenStream> {
    let (grammar, rule_enum, rule_variant) =
        top_level_attributes(PestAstAttribute::from_attributes(attrs)?)?;

    let (from_pest_lifetime, did_synthesize_lifetime) = generics
        .lifetimes()
        .next()
        .map(|def| (def.lifetime.clone(), false))
        .unwrap_or_else(|| (parse_quote!('unique_lifetime_name), true));

    let mut generics_ = generics.clone();
    let (_, ty_generics, where_clause) = generics.split_for_impl();
    if did_synthesize_lifetime {
        let lt = from_pest_lifetime.clone();
        generics_.params.push(parse_quote!(#lt));
    }
    let (impl_generics, _, _) = generics_.split_for_impl();

    let implementation = match data {
        Data::Union(data) => Err(Error::new(
            data.union_token.span(),
            "Cannot derive FromPest for union",
        )),
        Data::Struct(data) => derive_for_struct(grammar, &name, &rule_enum, &rule_variant, data),
        Data::Enum(data) => derive_for_enum(grammar, &name, &rule_enum, &rule_variant, data),
    }?;

    Ok(quote! {
        impl #impl_generics ::from_pest::FromPest<#from_pest_lifetime> for #name #ty_generics #where_clause {
            type Rule = #rule_enum;
            type Error = ::from_pest::NoneError;

            fn from_pest(
                pest: &mut ::from_pest::pest::iterators::Pairs<#from_pest_lifetime, #rule_enum>
            ) -> ::std::result::Result<Self, Self::Error> {
                #implementation
            }
        }
    })
}

#[derive(Clone, Debug)]
enum ConversionStrategy {
    FromPest,
    Outer(Vec<Path>),
    Inner(Vec<Path>),
}

impl ConversionStrategy {
    fn for_struct_field(span: Span, attrs: Vec<PestAstAttribute>) -> Result<Self> {
        let (strategy, _) = attrs
            .into_iter()
            .map(|attr| match attr {
                PestAstAttribute::Outer(_) => Ok((Some(ConversionStrategy::Outer(vec![])), None)),
                PestAstAttribute::Inner(_) => Ok((Some(ConversionStrategy::Inner(vec![])), None)),
                PestAstAttribute::With(attr) => Ok((None, Some(attr.path))),
                attr => Err(Error::new(attr.span(), "this attr not allowed here")),
            })
            .fold_results(
                Ok((ConversionStrategy::FromPest, vec![])),
                |acc: Result<_>, new| {
                    let acc = acc?;
                    Ok(match (acc, new) {
                        (_, (Some(_), Some(_))) | (_, (None, None)) => unreachable!(),
                        (_, (Some(ConversionStrategy::FromPest), _)) => unreachable!(),
                        ((ConversionStrategy::FromPest, mut old), (None, Some(new))) => {
                            old.push(new);
                            (ConversionStrategy::FromPest, old)
                        }
                        ((ConversionStrategy::FromPest, old), (Some(strat), None)) => (strat, old),
                        (
                            (ConversionStrategy::Outer(_), _),
                            (Some(ConversionStrategy::Outer(_)), _),
                        ) => Err(Error::new(
                            span,
                            "cannot specify `#[pest_ast(outer)]` more than once",
                        ))?,
                        (
                            (ConversionStrategy::Inner(_), _),
                            (Some(ConversionStrategy::Inner(_)), _),
                        ) => Err(Error::new(
                            span,
                            "cannot specify `#[pest_ast(inner)]` more than once",
                        ))?,
                        (
                            (ConversionStrategy::Outer(_), _),
                            (Some(ConversionStrategy::Inner(_)), _),
                        )
                        | (
                            (ConversionStrategy::Inner(_), _),
                            (Some(ConversionStrategy::Outer(_)), _),
                        ) => Err(Error::new(
                            span,
                            "cannot specify both `#[pest_ast(inner)]` and `#[pest::ast(outer)]`",
                        ))?,
                        ((ConversionStrategy::Outer(mut old), _), (None, Some(new))) => {
                            old.push(new);
                            (ConversionStrategy::Outer(old), vec![])
                        }
                        ((ConversionStrategy::Inner(mut old), _), (None, Some(new))) => {
                            old.push(new);
                            (ConversionStrategy::Inner(old), vec![])
                        }
                    })
                },
            )??;
        Ok(strategy)
    }
}

fn transform(stream: TokenStream, fns: Vec<Path>) -> TokenStream {
    fns.into_iter()
        .rev()
        .fold(stream, |stream, path| quote!(#path(#stream)))
}

fn derive_for_struct(
    grammar: Option<FilePath>,
    name: &Ident,
    rule_enum: &Path,
    rule_variant: &Ident,
    DataStruct { fields, .. }: DataStruct,
) -> Result<TokenStream> {
    if let Some(_path) = grammar {
        unimplemented!("Grammar introspection not implemented yet")
    }

    let (is_named, fields) = match fields {
        Fields::Named(fields) => (true, fields.named.into_iter()),
        Fields::Unnamed(fields) => (false, fields.unnamed.into_iter()),
        fields @ Fields::Unit => Err(Error::new(
            fields.span(),
            "Cannot derive FromPest for unit struct",
        ))?,
    };
    let fields: Vec<_> = fields
        .map(|field: Field| {
            let conversion = match ConversionStrategy::for_struct_field(
                field.span(),
                PestAstAttribute::from_attributes(field.attrs)?,
            )? {
                ConversionStrategy::FromPest => {
                    quote!(::from_pest::FromPest::from_pest(&mut inner)?)
                }
                ConversionStrategy::Inner(transformation) => {
                    let inner_str = quote!(inner.next().ok_or(::from_pest::NoneError)?.as_span());
                    transform(inner_str, transformation)
                }
                ConversionStrategy::Outer(transformation) => {
                    let outer_str = quote!(span.clone());
                    transform(outer_str, transformation)
                }
            };
            if false {
                #[allow(warnings)]
                let e: Error = unreachable!();
                #[allow(warnings)]
                Err(e) // type hint
            } else {
                Ok(if let Some(name) = field.ident {
                    quote!(#name : #conversion)
                } else {
                    quote!(#conversion)
                })
            }
        })
        .fold_results(vec![], |mut acc: Vec<_>, t| {
            acc.push(t);
            acc
        })?;

    let construct = if is_named {
        quote!(#name{#(#fields,)*})
    } else {
        quote!(#name(#(#fields),*))
    };

    Ok(quote! {
        let clone = pest.clone();
        let pair = pest.next().ok_or(::from_pest::NoneError)?;
        if pair.as_rule() == #rule_enum::#rule_variant {
            *pest = clone;
            let span = pair.as_span();
            let mut inner = pair.into_inner();
            let this = #construct;
            if inner.next().is_some() {
                panic!(
                    concat!(
                        "when converting ",
                        stringify!(#rule_variant),
                        ", found extraneous {:?}",
                    ),
                    inner,
                )
            }
            Ok(this)
        } else {
            Err(::from_pest::NoneError)
        }
    })
}

#[allow(unused)]
#[allow(clippy::needless_pass_by_value)]
fn derive_for_enum(
    grammar: Option<FilePath>,
    name: &Ident,
    rule_enum: &Path,
    rule_variant: &Ident,
    DataEnum { variants, .. }: DataEnum,
) -> Result<TokenStream> {
    unimplemented!("Enums not reimplemented yet")
}
