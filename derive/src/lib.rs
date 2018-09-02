#![allow(non_snake_case)]
#![recursion_limit = "128"]

extern crate itertools;
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate single;
#[macro_use]
extern crate quote;

use itertools::Itertools;
use proc_macro2::Span;
use single::Single;
use syn::{
    spanned::Spanned, synom::Synom, Data, DataStruct, DeriveInput, Field, Fields, Ident, Path,
    Type, TypePath, Attribute, Meta, NestedMeta,
};

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

    let impl_hiding_const = syn::Ident::new(
        &format!("__IMPL_FromPest_FOR_{}", name.to_string()),
        Span::call_site(),
    );

    let lifetime = {
        let mut lifetimes = input.generics.lifetimes();
        match (lifetimes.next(), lifetimes.next()) {
            (Some(def), None) => def.lifetime.clone(),
            _ => Err((
                "FromPest can only be derived for a struct with a single lifetime parameter"
                    .to_string(),
                input.generics.params.span(),
            ))?,
        }
    };

    let rule_enum = {
        // syn::Meta doesn't handle `#[pest(rule = Rule)]` so do it manually
        struct RuleMeta(Path);
        impl Synom for RuleMeta {
            named!(parse -> Self, do_parse!(
                it: parens!(do_parse!(
                    custom_keyword!(rule) >>
                    punct!(=) >>
                    path: syn!(Path) >>
                    (path)
                )) >>
                (RuleMeta(it.1))
            ));
        }

        let mut rule_metas = input
            .attrs
            .iter()
            .filter(|attr| {
                attr.path
                    .segments
                    .iter()
                    .single()
                    .ok()
                    .filter(|segment| segment.ident == "pest")
                    .is_some()
            })
            .filter_map(|attr| syn::parse2::<RuleMeta>(attr.tts.clone()).ok())
            .fuse();

        match (rule_metas.next(), rule_metas.next()) {
            (Some(RuleMeta(path)), None) => path,
            (_, Some(RuleMeta(path))) => Err((
                "FromPest requires exactly one `#[pest(rule = Rule)]` attribute".to_string(),
                path.span(),
            ))?,
            (None, None) => Err((
                "FromPest requires exactly one `#[pest(rule = Rule)]` attribute".to_string(),
                Span::call_site(),
            ))?,
        }
    };

    let implementation = match input.data {
        Data::Struct(data) => derive_FromPest_DataStruct(name.clone(), data)?,
        _ => unimplemented!(),
    };

    Ok(quote! {
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const #impl_hiding_const: () = {
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            #[allow(rust_2018_idioms)]
            extern crate pest_deconstruct as __crate;
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            #[allow(rust_2018_idioms)]
            extern crate pest as __pest;
            impl #impl_generics __crate::FromPest < #lifetime > for #name #type_generics #where_clause {
                type Rule = #rule_enum;
                const RULE: #rule_enum = #rule_enum::#name;
                fn from_pest(pest: __pest::iterators::Pair<#lifetime, #rule_enum>) -> Self {
                    #[allow(unused)]
                    #[allow(deprecated)]
                    let span = pest.clone().into_span();
                    #[allow(unused)]
                    let mut it = __crate::PestDeconstruct::deconstruct(pest);

                    #implementation
                }
            }
        };
    })
}

fn derive_FromPest_DataStruct(name: Ident, input: DataStruct) -> DeriveResult {
    fn deconstruct_to_field(field: &Field) -> DeriveResult {
        fn type_path(ty: &Type) -> Result<&TypePath> {
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
                Type::Paren(ty) => type_path(&ty.elem),
                Type::Group(ty) => type_path(&ty.elem),
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

        let ty = type_path(&field.ty)?;
        if ty.qself.is_some() {
            Err(("FromPest derive does not support qualified self typed fields".to_string(), ty.span()))?;
        }

        let segment = ty.path.segments.iter().next().unwrap();
        let span = segment.span();
        let name = &field.ident;

        let parse = field.attrs
            .iter()
            .filter_map(Attribute::interpret_meta)
            .filter_map(|meta| {
                match meta {
                    Meta::List(meta) => if meta.ident == "pest" {
                        Some(meta)
                    } else {
                        None
                    },
                    _ => None,
                }
            })
            .filter_map(|meta| meta.nested.iter().single().ok().cloned())
            .filter_map(|meta| match meta {
                NestedMeta::Meta(meta) => Some(meta),
                _ => None,
            })
            .filter_map(|meta| match meta {
                Meta::Word(ident) => Some(ident),
                _ => None,
            })
            .any(|ident| ident == "parse");

        let translation = if parse {
            quote_spanned! {span=>
                span.as_str().parse().unwrap()
            }
        } else if segment.ident == "Box" {
            quote_spanned! {span=>
                Box::new(it.next())
            }
        } else if segment.ident == "Vec" {
            quote_spanned! {span=>
                it.next_many()
            }
        } else if segment.ident == "Option" {
            quote_spanned! {span=>
                it.next_opt()
            }
        } else if segment.ident == "Span" {
            quote_spanned! {span=>
                span.into()
            }
        } else {
            quote_spanned! {span=>
                it.next()
            }
        };

        Ok(quote!(#(#name:)* #translation))
    }

    fn accumulate<T>(mut acc: Vec<T>, item: T) -> Vec<T> {
        acc.push(item);
        acc
    }

    match input.fields {
        Fields::Named(fields) => {
            let fields = fields
                .named
                .iter()
                .map(deconstruct_to_field)
                .fold_results(vec![], accumulate)?;
            Ok(quote!(#name { #(#fields),* } ))
        }
        Fields::Unnamed(fields) => {
            let fields = fields
                .unnamed
                .iter()
                .map(deconstruct_to_field)
                .fold_results(vec![], accumulate)?;
            Ok(quote!(#name(#(#fields),*)))
        }
        Fields::Unit => Ok(quote!(#name)),
    }
}
