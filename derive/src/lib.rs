#![allow(non_snake_case)]
#![recursion_limit = "150"]

extern crate itertools;
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

#[allow(non_snake_case)]
#[proc_macro_derive(FromPest, attributes(pest_ast))]
pub fn derive_FromPest(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    //    let x =
    syn::parse(input)
        .and_then(from_pest::derive)
        .unwrap_or_else(|err| err.to_compile_error())
        //        .to_string();
        //    quote!(compile_error!(#x);)
        .into()
}

mod attributes;
mod from_pest;

#[cfg(feature = "trace")]
fn trace(t: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! { ::from_pest::log::trace!( #t ); }
}

#[cfg(not(feature = "trace"))]
fn trace(_t: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {}
}
