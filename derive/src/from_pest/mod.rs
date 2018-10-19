//! Machinery in charge of deriving `FromPest` for a type.
//!
//! Documentation in this module and submodules describes the requirement placed on _child_
//! functions. This is important as manipulation is done over untyped `TokenStream`.

use {
    itertools::Itertools,
    proc_macro2::{Span, TokenStream},
    quote::ToTokens,
    std::path::PathBuf as FilePath,
    syn::{
        parse::Error, parse::Result, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput,
        Ident, Path,
    },
};

use attributes::PestAstAttribute;

mod field;

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

/// Creates implementation of `FromPest` for given derive input.
///
/// For child functions, sets up an environment with:
///
/// ```text
/// type Self::Rule;
/// type Self::FatalError = Void;
/// let pest: &mut Pairs;
/// ```
///
/// Child function is required to produce a number of statements that implement the semantics of
/// `FromPest::from_pest`; that is: `Ok(Self)` => success, `pest` updated past the node;
/// `Err(NoMatch)` => failure, `pest` is unchanged; `Err(Malformed)` impossible, panic instead.
/// `?` and `return` may be used for early exit of failed matches.
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
            type FatalError = ::from_pest::Void;

            fn from_pest(
                pest: &mut ::from_pest::pest::iterators::Pairs<#from_pest_lifetime, #rule_enum>
            ) -> ::std::result::Result<Self, ::from_pest::ConversionError<::from_pest::Void>> {
                #implementation
            }
        }
    })
}

/// Implements `FromPest::from_pest` for some struct.
///
/// For child functions, sets up an environment with:
///
/// ```text
/// let span: Span;         // the span of this production
/// let inner: &mut Pairs;  // the contents of this production
/// ```
///
/// Child function is required to produce an _expression_ which constructs an instance of `Self`
/// from the `Pair`s in `inner` or early returns a `NoMatch`. `inner` and `span` are free working
/// space, but `inner` should represent the point past all consumed productions afterwards.
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

    let construct = field::convert(name, fields)?;

    Ok(quote! {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(::from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == #rule_enum::#rule_variant {
            let span = pair.as_span();
            let mut inner = pair.into_inner();
            let inner = &mut inner;
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
            *pest = clone;
            Ok(this)
        } else {
            Err(::from_pest::ConversionError::NoMatch)
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
