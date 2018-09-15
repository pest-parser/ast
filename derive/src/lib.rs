#![allow(non_snake_case)]
#![recursion_limit = "128"]

extern crate itertools;
extern crate proc_macro;
extern crate proc_macro2;
extern crate single;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use itertools::Itertools;
use proc_macro2::Span;
use single::Single;
use syn::{
    punctuated::Pair, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput, Field, Fields,
    Ident, Path, Type, TypePath,
};

mod attributes;
mod utils;

use attributes::{pest_attributes, PestAttribute};
use utils::accumulate;

type Result<T> = std::result::Result<T, (String, Span)>;
type DeriveResult = Result<proc_macro2::TokenStream>;

#[proc_macro_derive(FromPest, attributes(pest))]
pub fn derive_FromPest(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_FromPest_impl(syn::parse(input).unwrap())
        .unwrap_or_else(compile_error)
        .into()
}

fn compile_error((err, span): (String, Span)) -> proc_macro2::TokenStream {
    quote_spanned! {span=>
        compile_error!(#err);
    }
}

fn derive_FromPest_impl(input: DeriveInput) -> DeriveResult {
    let name = input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let impl_scoping_const = syn::Ident::new(
        &format!("__IMPL_FromPest_FOR_{}", name.to_string()),
        Span::call_site(),
    );

    let lifetime = {
        let mut lifetimes = input.generics.lifetimes();
        match (lifetimes.next(), lifetimes.next()) {
            (Some(def), None) => def.lifetime.clone(),
            (Some(_), Some(_)) => Err((
                "FromPest cannot be derived on a struct with more than one lifetime parameter"
                    .to_string(),
                input.generics.params.span(),
            ))?,
            (None, _) => parse_quote!('i),
        }
    };

    let attrs = pest_attributes(&input.attrs)?;

    let (rule_enum, rule_variant) = {
        let rule_variant = attrs
            .iter()
            .filter_map(PestAttribute::rule)
            .single()
            .map_err(|err| {
                (
                    format!(
                        "Deriving FromPest requires a single `#[pest(rule = <Rule>)]`, \
                         you provided {}",
                        match err {
                            single::Error::NoElements => "none",
                            single::Error::MultipleElements => "multiple",
                        }
                    ),
                    Span::call_site(),
                )
            })?;
        let mut rule_enum: Path = rule_variant.clone();
        rule_enum.segments.pop();
        match rule_enum.segments.pop() {
            Some(Pair::Punctuated(t, _)) | Some(Pair::End(t)) => rule_enum.segments.push_value(t),
            None => Err((
                "Path should be to the enum variant, including the enum in the path".to_string(),
                rule_variant.span(),
            ))?,
        }
        (rule_enum, rule_variant)
    };

    let discard = match attrs.iter().filter(|attr| attr.discard_trailing()).count() {
        0 => quote!(),
        1 => quote!(it.discard();),
        _ => Err((
            "Multiple `#[pest(discard_trailing)]` attributes are not allowed".to_string(),
            Span::call_site(),
        ))?,
    };

    let implementation = match input.data {
        Data::Struct(data) => derive_FromPest_DataStruct(name.clone(), data)?,
        Data::Enum(data) => derive_FromPest_DataEnum(name.clone(), data)?,
        Data::Union(data) => Err((
            "FromPest cannot be derived for union types".to_string(),
            data.union_token.span(),
        ))?,
    };

    Ok(quote! {
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const #impl_scoping_const: () = {
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            #[allow(rust_2018_idioms)]
            extern crate pest_deconstruct as __crate;
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            #[allow(rust_2018_idioms)]
            extern crate pest as __pest;
            impl #impl_generics __crate::FromPest < #lifetime > for #name #type_generics #where_clause {
                type Rule = #rule_enum;
                const RULE: #rule_enum = #rule_variant;
                fn from_pest(pest: __pest::iterators::Pair<#lifetime, #rule_enum>) -> Self {
                    #[allow(unused)]
                    let span = pest.as_span();
                    #[allow(unused)]
                    let mut it = __crate::PestDeconstruct::deconstruct(pest);

                    let result = #implementation;
                    #discard
                    result
                }
            }
        };
    })
}

fn type_path_field(ty: &Type) -> Result<&TypePath> {
    match ty {
        Type::Slice(ty) => Err((
            "FromPest derive does not support slice fields".to_string(),
            ty.span(),
        )),
        Type::Array(ty) => Err((
            "FromPest derive does not support array fields".to_string(),
            ty.span(),
        )),
        Type::Ptr(ty) => Err((
            "FromPest derive does not support ptr fields".to_string(),
            ty.span(),
        )),
        Type::Reference(ty) => Err((
            "FromPest derive does not support reference fields".to_string(),
            ty.span(),
        )),
        Type::BareFn(ty) => Err((
            "FromPest derive does not support bare fn fields".to_string(),
            ty.span(),
        )),
        Type::Never(ty) => Err((
            "FromPest derive does not support ! fields".to_string(),
            ty.span(),
        )),
        Type::Tuple(ty) => Err((
            "FromPest derive does not support tuple fields; use separate fields instead"
                .to_string(),
            ty.span(),
        )),
        Type::Path(ty) => Ok(ty),
        Type::TraitObject(ty) => Err((
            "FromPest derive does not support trait object fields".to_string(),
            ty.span(),
        )),
        Type::ImplTrait(ty) => Err((
            "FromPest derive does not support impl trait fields".to_string(),
            ty.span(),
        )),
        Type::Paren(ty) => type_path_field(&ty.elem),
        Type::Group(ty) => type_path_field(&ty.elem),
        Type::Infer(ty) => Err((
            "FromPest derive does not support inferred fields".to_string(),
            ty.span(),
        )),
        Type::Macro(ty) => Err((
            "FromPest derive does not support macro typed fields".to_string(),
            ty.span(),
        )),
        Type::Verbatim(ty) => Err((
            "FromPest derive has no idea what type field this is".to_string(),
            ty.span(),
        )),
    }
}

enum ParseKind {
    Outer,
    Inner(Path),
}

fn should_do_parse(span: Span, attrs: &[PestAttribute]) -> Result<Option<ParseKind>> {
    Ok(match attrs.iter().filter(|attr| attr.parse()).count() {
        0 => None,
        1 => match attrs.iter().flat_map(|attr| attr.rule()).single() {
            Ok(path) => Some(ParseKind::Inner(path.clone())),
            Err(single::Error::NoElements) => Some(ParseKind::Outer),
            Err(single::Error::MultipleElements) => Err((
                "Multiple `#[pest(rule = <Rule>)]` are not allowed".to_string(),
                span,
            ))?,
        },
        _ => Err((
            "Multiple `#[pest(parse)]` attributes are not allowed".to_string(),
            Span::call_site(),
        ))?,
    })
}

fn derive_FromPest_DataStruct(name: Ident, input: DataStruct) -> DeriveResult {
    fn deconstruct_field(field: &Field) -> DeriveResult {
        let ty = type_path_field(&field.ty)?;
        if ty.qself.is_some() {
            Err((
                "FromPest derive does not support qualified self typed fields".to_string(),
                ty.span(),
            ))?;
        }

        let segment = ty.path.segments.iter().next().unwrap();
        let span = segment.span();
        let name = &field.ident;
        let attrs = pest_attributes(&field.attrs)?;

        let translation = if segment.ident == "Box" {
            match should_do_parse(field.span(), &attrs)? {
                None => quote_spanned! {span=>
                    Box::new(it.next())
                },
                Some(ParseKind::Outer) => quote_spanned! {span=>
                    Box::new(span.as_str().parse().unwrap())
                },
                Some(ParseKind::Inner(rule)) => quote_spanned! {span=>
                    Box::new(it.next_pair(#rule).as_span().as_str().parse().unwrap())
                },
            }
        } else if segment.ident == "Vec" {
            match should_do_parse(field.span(), &attrs)? {
                None => quote_spanned! {span=>
                    it.next_many()
                },
                Some(ParseKind::Outer) => Err((
                    "It doesn't make sense to outer parse into a Vec; maybe provide a rule"
                        .to_string(),
                    span,
                ))?,
                Some(ParseKind::Inner(rule)) => quote_spanned! {span=>
                    it.next_pair_many(#rule)
                        .into_iter()
                        .map(|pair| pair.as_span().as_str().parse().unwrap())
                        .collect()
                },
            }
        } else if segment.ident == "Option" {
            match should_do_parse(field.span(), &attrs)? {
                None => quote_spanned! {span=>
                    it.next_opt()
                },
                Some(ParseKind::Outer) => quote_spanned! {span=>
                    span.as_str().parse().ok()
                },
                Some(ParseKind::Inner(rule)) => quote_spanned! {span=>
                    it.next_pair_opt(#rule)
                        .and_then(|pair| pair.as_span().as_str().parse().ok())
                },
            }
        } else if segment.ident == "Span" {
            match should_do_parse(field.span(), &attrs)? {
                None => quote_spanned! {span=>
                    span.clone().into()
                },
                Some(ParseKind::Outer) => Err((
                    "It doesn't make sense to outer parse into a Span; just use Span or provide a rule"
                        .to_string(),
                    span,
                ))?,
                Some(ParseKind::Inner(rule)) => quote_spanned! {span=>
                    it.next_pair(#rule).as_span().into()
                },
            }
        } else {
            match should_do_parse(field.span(), &attrs)? {
                None => quote_spanned! {span=>
                    it.next()
                },
                Some(ParseKind::Outer) => quote_spanned! {span=>
                    span.as_str().parse().unwrap()
                },
                Some(ParseKind::Inner(rule)) => quote_spanned! {span=>
                    it.next_pair(#rule).as_span().as_str().parse().unwrap()
                },
            }
        };

        Ok(quote!(#(#name:)* #translation))
    }

    match input.fields {
        Fields::Named(fields) => {
            let fields = fields
                .named
                .iter()
                .map(deconstruct_field)
                .fold_results(vec![], accumulate)?;
            Ok(quote!(#name { #(#fields),* } ))
        }
        Fields::Unnamed(fields) => {
            let fields = fields
                .unnamed
                .iter()
                .map(deconstruct_field)
                .fold_results(vec![], accumulate)?;
            Ok(quote!(#name(#(#fields),*)))
        }
        Fields::Unit => Ok(quote!(#name)),
    }
}

fn derive_FromPest_DataEnum(name: Ident, input: DataEnum) -> DeriveResult {
    let variants = input.variants.iter().map(|variant| {
        let attrs = pest_attributes(&variant.attrs)?;
        let ident = &variant.ident;

        match should_do_parse(variant.span(), &attrs)? {
            None => Ok(quote! {
                if let Some(node) = it.next_opt() {
                    #name::#ident(node)
                }
            }),
            Some(ParseKind::Outer) => Ok(quote! {
                if let Ok(node) = span.as_str().parse() {
                    #name::#ident(node)
                }
            }),
            Some(ParseKind::Inner(rule)) => Ok(quote! {
                if let Some(Ok(node)) = it.next_pair_opt(#rule)
                    .map(|pair| pair.as_span().as_str().parse())
                {
                    it.next_untyped();
                    #name::#ident(node)
                }
            }),
        }
    }).fold_results(vec![], accumulate)?;

    Ok(quote! {
        #(#variants)else* else {
            panic!(
                "Unexpected {}{:?}",
                stringify!(#name),
                ::std::iter::repeat_with(|| it.next_untyped())
                    .take_while(Option::is_some)
                    .map(Option::unwrap)
                    .collect::<Vec<_>>(),
            )
        }
    })
}
