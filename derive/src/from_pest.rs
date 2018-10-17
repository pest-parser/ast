#![allow(clippy::needless_pass_by_value)]

use {
    itertools::Itertools,
    proc_macro2::{Span, TokenStream},
    quote::ToTokens,
    std::{path::PathBuf as FilePath},
    syn::{
        parse::Error, parse::Result, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput,
        GenericParam, Ident, Path,
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
            "Exactly one `#[pest::ast(rule(path::to))]` required",
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
        .params
        .iter()
        .filter_map(|generic| match generic {
            GenericParam::Lifetime(def) => Some(def.lifetime.clone()),
            _ => None,
        })
        .next()
        .map(|lt| (lt, true))
        .unwrap_or_else(|| (parse_quote!('unique_lifetime_name), false));

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
        Data::Struct(data) => {
            derive_for_struct(grammar, name.clone(), rule_enum.clone(), rule_variant, data)
        }
        Data::Enum(data) => {
            derive_for_enum(grammar, name.clone(), rule_enum.clone(), rule_variant, data)
        }
    }?;

    Ok(quote! {
        impl #impl_generics ::from_pest::FromPest<#from_pest_lifetime> for #name #ty_generics #where_clause {
            type Rule: ::from_pest::pest::RuleType = #rule_enum;
            type Error = ::from_pest::void::Void;

            fn from_pest(
                pest: &mut ::from_pest::pest::iterators::Pairs<#from_pest_lifetime, #rule_enum>
            ) -> ::std::result::Result<Self, Self::Error> {
                #implementation
            }
        }
    })
}

#[allow(unused)]
fn derive_for_struct(
    grammar: Option<FilePath>,
    name: Ident,
    rule_enum: Path,
    rule_variant: Ident,
    DataStruct { fields, .. }: DataStruct,
) -> Result<TokenStream> {
    unimplemented!("Enums not reimplemented yet")
}


#[allow(unused)]
fn derive_for_enum(
    grammar: Option<FilePath>,
    name: Ident,
    rule_enum: Path,
    rule_variant: Ident,
    DataEnum { variants, .. }: DataEnum,
) -> Result<TokenStream> {
    unimplemented!("Enums not reimplemented yet")
}
