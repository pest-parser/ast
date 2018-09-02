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
    spanned::Spanned, Attribute, Data, DataStruct, DeriveInput, Field, Fields, Ident, Lit, Meta,
    NestedMeta, Path, Type, TypePath,
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

    let rule_enum: Path = {
        input
            .attrs
            .iter()
            .filter_map(Attribute::interpret_meta)
            .flat_map(extract_pest_meta)
            .filter_map(extract_assigned_rule)
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
            })??
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

fn extract_pest_meta(meta: Meta) -> syn::punctuated::IntoIter<NestedMeta, Token![,]> {
    match meta {
        Meta::List(meta) => {
            if meta.ident == "pest" {
                return meta.nested.into_iter();
            }
        }
        _ => {}
    };
    syn::punctuated::Punctuated::new().into_iter()
}

fn extract_assigned_rule(meta: NestedMeta) -> Option<Result<Path>> {
    match meta {
        NestedMeta::Meta(Meta::NameValue(meta)) => if meta.ident == "rule" {
            return Some(lit_path_or_err(meta.lit));
        },
        _ => {}
    }
    None
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

fn lit_path_or_err(lit: Lit) -> Result<Path> {
    match lit {
        Lit::Str(lit) => Ok(lit
            .parse()
            .map_err(|_| (("Expected a path, parse failed".to_string(), lit.span())))?),
        _ => Err((
            "`#[pest(rule = <Rule>)]` requires a string literal path".to_string(),
            lit.span(),
        )),
    }
}

enum ParseKind {
    Outer,
    Inner(Path),
}

fn should_do_parse(field: &Field) -> Result<Option<ParseKind>> {
    let pest_meta = field
        .attrs
        .iter()
        .filter_map(Attribute::interpret_meta)
        .flat_map(extract_pest_meta)
        .collect_vec();

    let do_parse = pest_meta.iter().any(|meta| match meta {
        NestedMeta::Meta(Meta::Word(ident)) if ident == "parse" => true,
        _ => false,
    });

    if do_parse {
        let rule = pest_meta
            .into_iter()
            .filter_map(extract_assigned_rule)
            .single();

        match rule {
            Ok(ident) => Ok(Some(ParseKind::Inner(ident?))),
            Err(single::Error::NoElements) => Ok(Some(ParseKind::Outer)),
            Err(single::Error::MultipleElements) => Err((
                "Found multiple conflicting `#[pest(rule = <Rule>)]` on field".to_string(),
                field.span(),
            )),
        }
    } else {
        Ok(None)
    }
}

fn derive_FromPest_DataStruct(name: Ident, input: DataStruct) -> DeriveResult {
    fn deconstruct_to_field(field: &Field) -> DeriveResult {
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

        let translation = if let Some(kind) = should_do_parse(field)? {
            match kind {
                ParseKind::Outer => {
                    quote_spanned! {span=>
                        span.clone().as_str().parse().unwrap()
                    }
                }
                ParseKind::Inner(rule) => {
                    quote_spanned! {span=>
                        it.next_pair(#rule).into_span().as_str().parse().unwrap()
                    }
                }
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
                span.clone().into()
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
