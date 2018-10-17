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

#[allow(non_snake_case)]
#[proc_macro_derive(FromPest, attributes(pest_ast))]
pub fn derive_FromPest(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse(input)
        .and_then(from_pest::derive)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

mod attributes;
mod from_pest;
