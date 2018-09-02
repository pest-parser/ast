#![allow(non_snake_case)]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
extern crate single;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate matches;

use proc_macro2::Span;
use single::Single;
use syn::synom::Synom;
use syn::{spanned::Spanned, Data, DataStruct, DeriveInput, Ident, Path};

type DeriveResult = Result<proc_macro2::TokenStream, (String, Span)>;

#[proc_macro_derive(FromPest)]
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
        Data::Struct(data) => derive_FromPest_DataStruct(name.clone(), rule_enum, data)?,
        _ => unimplemented!(),
    };

    Ok(quote! {
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const #impl_hiding_const: () = {
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            #[allow(rust_2018_idioms)]
            extern crate pest_deconstruct as __crate;
            impl #impl_generics __crate::FromPest < #lifetime > for #name #type_generics #where_clause {
                #implementation
            }
        };
    })
}

fn derive_FromPest_DataStruct(name: Ident, rule_enum: Path, input: DataStruct) -> DeriveResult {
    unimplemented!()
}
