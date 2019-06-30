//! Machinery in charge of deriving `FromPest` for a type.
//!
//! Documentation in this module and submodules describes the requirement placed on _child_
//! functions. This is important as manipulation is done over untyped `TokenStream`.

use {
    proc_macro2::TokenStream,
    std::{path::PathBuf as FilePath},
    syn::{
        parse::Error, parse::Result, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput,
        Ident, Path,
    },
};

use attributes::DeriveAttribute;

mod field;

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
    let attrs = DeriveAttribute::from_attributes(attrs)?;

    let grammar = {
        let mut grammar_attrs = attrs.iter().filter_map(|attr| match attr {
            DeriveAttribute::Grammar(attr) => Some(attr),
            _ => None,
        });
        match (grammar_attrs.next(), grammar_attrs.next()) {
            (Some(_), Some(attr)) => Err(Error::new(
                attr.span(),
                "duplicate #[pest_ast(grammar)] not allowed",
            ))?,
            (None, None) => None,
            (Some(attr), None) => Some(FilePath::from(attr.lit.value())),
            _ => unreachable!(),
        }
    };

    let (rule_enum, rule_variant) = {
        let mut rule_attrs = attrs.into_iter().filter_map(|attr| match attr {
            DeriveAttribute::Rule(attr) => Some(attr),
            _ => None,
        });
        match (rule_attrs.next(), rule_attrs.next()) {
            (Some(_), Some(attr)) => Err(Error::new(
                attr.span(),
                "duplicate #[pest_ast(rule)] not allowed",
            ))?,
            (None, None) => Err(Error::new(name.span(), "#[pest_ast(rule)] required here"))?,
            (Some(attr), None) => (attr.path, attr.variant),
            _ => unreachable!(),
        }
    };

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

    let construct = field::convert(&parse_quote!(#name), fields)?;

    let extraneous =
        ::trace(quote! { "when converting {}, found extraneous {:?}", stringify!(#name), inner});

    Ok(quote! {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(::from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == #rule_enum::#rule_variant {
            let span = pair.as_span();
            let mut inner = pair.into_inner();
            let inner = &mut inner;
            let this = #construct;
            if inner.clone().next().is_some() {
                #extraneous
                Err(::from_pest::ConversionError::Extraneous {
                    current_node: stringify!(#name),
                })?;
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
    if let Some(_path) = grammar {
        unimplemented!("Grammar introspection not implemented yet")
    }

    let convert_variants: Vec<TokenStream> = variants
        .into_iter()
        .map(|variant| {
            let variant_name = variant.ident;
            let construct_variant = field::convert(&parse_quote!(#name::#variant_name), variant.fields)?;
            let extraneous = ::trace(quote! {
                "when converting {}, found extraneous {:?}", stringify!(#name), stringify!(#variant_name)
            });

            Ok(quote! {
                let span = pair.as_span();
                let mut inner = pair.clone().into_inner();
                let inner = &mut inner;
                let this = #construct_variant;
                if inner.clone().next().is_some() {
                    #extraneous
                    Err(::from_pest::ConversionError::Extraneous {
                        current_node: stringify!(#variant_name),
                    })?;
                }
                Ok(this)
            })
        })
        .collect::<Result<_>>()?;

    Ok(quote! {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(::from_pest::ConversionError::NoMatch)?;
        if pair.as_rule() == #rule_enum::#rule_variant {
            let this = Err(::from_pest::ConversionError::NoMatch)
                #(.or_else(|_: ::from_pest::ConversionError<::from_pest::Void>| {
                    #convert_variants
                }))*?;
            *pest = clone;
            Ok(this)
        } else {
            Err(::from_pest::ConversionError::NoMatch)
        }
    })
}
