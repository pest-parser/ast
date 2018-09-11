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
    punctuated::Pair, spanned::Spanned, Attribute, Data, DataEnum, DataStruct, DeriveInput, Field,
    Fields, Ident, Lit, Meta, NestedMeta, Path, Type, TypePath,
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

    let (rule_enum, rule_variant) = {
        let rule_variant: Path = input
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
            })??;
        let mut rule_enum: Path = rule_variant.clone();
        rule_enum.segments.pop();
        if let Some(pair) = rule_enum.segments.pop() {
            // reattach final path segment without trailing punct
            match pair {
                Pair::Punctuated(t, _) | Pair::End(t) => rule_enum.segments.push_value(t),
            }
        } else {
            Err((
                "`#[pest(rule = <Rule>)]` should take the path to the enum variant, \
                 including the enum"
                    .to_string(),
                rule_variant.span(),
            ))?;
        }
        (rule_enum, rule_variant)
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
        const #impl_hiding_const: () = {
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
                    #[allow(deprecated)]
                    let span = pest.as_span();
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

fn accumulate<T>(mut acc: Vec<T>, item: T) -> Vec<T> {
    acc.push(item);
    acc
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

        let translation = if segment.ident == "Box" {
            match should_do_parse(field)? {
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
            match should_do_parse(field)? {
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
                        .map(|pair| pair.as_span().as_str().parse().unwrap())
                        .collect()
                },
            }
        } else if segment.ident == "Option" {
            match should_do_parse(field)? {
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
            match should_do_parse(field)? {
                None => quote_spanned! {span=>
                    span.clone().into()
                },
                Some(ParseKind::Outer) => Err((
                    "It doesn't make sense to outer parse into a Span; just use Span or provide a rule"
                        .to_string(),
                    span,
                ))?,
                Some(ParseKind::Inner(rule)) => quote_spanned! {span=>
                    it.next_pair(#rule).as_span()
                },
            }
        } else {
            match should_do_parse(field)? {
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
        let variant = &variant.ident;
        quote! {
            if let Some(node) = it.next_opt() {
                #name::#variant(node)
            }
        }
    });

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
