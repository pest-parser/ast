pub fn accumulate<T>(mut acc: Vec<T>, item: T) -> Vec<T> {
    acc.push(item);
    acc
}

pub fn adapt_error(error: ::syn::parse::Error) -> (String, ::proc_macro2::Span) {
    (error.to_string(), error.span())
}
