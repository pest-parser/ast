#![allow(clippy::mixed_read_write_in_expression)] // syn patterns

use proc_macro2::TokenTree;

use {
    itertools::Itertools,
    proc_macro2::TokenStream,
    quote::ToTokens,
    syn::{
        parse::{Error, Parse, ParseStream, Parser, Result},
        punctuated::Punctuated,
        spanned::Spanned,
        token::Paren,
        Attribute, Ident, LitStr, Path,
    },
};

mod kw {
    custom_keyword!(grammar);
    custom_keyword!(outer);
    custom_keyword!(inner);
    custom_keyword!(with);
    custom_keyword!(rule);
}

/// `#[pest_ast(..)]` for the outer `#[derive(FromPest)]`
#[derive(Debug)]
pub(crate) enum DeriveAttribute {
    /// `grammar = "grammar.rs"`
    Grammar(GrammarAttribute),
    /// `rule(path::to)`
    Rule(RuleAttribute),
}

/// `#[pest_ast(..)]` for fields in `#[derive(FromPest)]`
pub(crate) enum FieldAttribute {
    /// `outer(with(path::to),*)`
    Outer(OuterAttribute),
    /// `inner(rule(path::to), with(path::to),*)`
    Inner(InnerAttribute),
}

#[derive(Debug)]
pub(crate) struct GrammarAttribute {
    pub(crate) grammar: kw::grammar,
    pub(crate) eq: Token![=],
    pub(crate) lit: LitStr,
}

pub(crate) struct OuterAttribute {
    pub(crate) outer: kw::outer,
    pub(crate) paren: Paren,
    pub(crate) with: Punctuated<WithAttribute, Token![,]>,
}

pub(crate) struct InnerAttribute {
    pub(crate) inner: kw::inner,
    pub(crate) paren: Paren,
    pub(crate) rule: Option<RuleAttribute>,
    pub(crate) comma: Option<Token![,]>,
    pub(crate) with: Punctuated<WithAttribute, Token![,]>,
}

pub(crate) struct WithAttribute {
    pub(crate) with: kw::with,
    pub(crate) paren: Paren,
    pub(crate) path: Path,
}

#[derive(Debug)]
pub(crate) struct RuleAttribute {
    pub(crate) rule: kw::rule,
    pub(crate) paren: Paren,
    pub(crate) path: Path,
    pub(crate) sep: Token![::],
    pub(crate) variant: Ident,
}

impl DeriveAttribute {
    pub(crate) fn from_attributes(attrs: impl IntoIterator<Item = Attribute>) -> Result<Vec<Self>> {
        attrs
            .into_iter()
            .map(DeriveAttribute::from_attribute)
            .fold_ok(vec![], |mut acc, t| {
                acc.extend(t);
                acc
            })
    }

    pub(crate) fn from_attribute(attr: Attribute) -> Result<Vec<Self>> {
        if attr.path() != &parse_quote!(pest_ast) {
            return Ok(vec![]);
        }

        Parser::parse2(
            |input: ParseStream| {
                let content;
                input.parse::<Token![#]>()?;
                bracketed!(content in input);
                content.step(|cursor| {
                    if let Some((tt, next)) = cursor.token_tree() {
                        match tt {
                            TokenTree::Ident(id) if id == "pest_ast" => Ok(((), next)),
                            _ => Err(cursor.error("expected `pest_ast`")),
                        }
                    } else {
                        Err(cursor.error("expected `pest_ast`"))
                    }
                })?;
                let content2;
                parenthesized!(content2 in content);
                let punctuated: Punctuated<_, Token![,]> =
                    content2.parse_terminated(Parse::parse, Token![,])?;
                Ok(punctuated.into_iter().collect_vec())
            },
            attr.to_token_stream(),
        )
    }
}

impl FieldAttribute {
    pub(crate) fn from_attributes(attrs: impl IntoIterator<Item = Attribute>) -> Result<Vec<Self>> {
        attrs
            .into_iter()
            .map(FieldAttribute::from_attribute)
            .fold_ok(vec![], |mut acc, t| {
                acc.extend(t);
                acc
            })
    }

    pub(crate) fn from_attribute(attr: Attribute) -> Result<Vec<Self>> {
        if attr.path() != &parse_quote!(pest_ast) {
            return Ok(vec![]);
        }

        Parser::parse2(
            |input: ParseStream| {
                input.parse::<Token![#]>()?;
                let content;
                bracketed!(content in input);
                content.step(|cursor| {
                    if let Some((tt, next)) = cursor.token_tree() {
                        match tt {
                            TokenTree::Ident(id) if id == "pest_ast" => Ok(((), next)),
                            _ => Err(cursor.error("expected `pest_ast`")),
                        }
                    } else {
                        Err(cursor.error("expected `pest_ast`"))
                    }
                })?;
                let content2;
                parenthesized!(content2 in content);
                let punctuated: Punctuated<_, Token![,]> =
                    content2.parse_terminated(Parse::parse, Token![,])?;
                Ok(punctuated.into_iter().collect_vec())
            },
            attr.to_token_stream(),
        )
    }
}

impl Parse for DeriveAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::grammar) {
            GrammarAttribute::parse(input).map(DeriveAttribute::Grammar)
        } else if lookahead.peek(kw::rule) {
            RuleAttribute::parse(input).map(DeriveAttribute::Rule)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for FieldAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::outer) {
            OuterAttribute::parse(input).map(FieldAttribute::Outer)
        } else if lookahead.peek(kw::inner) {
            InnerAttribute::parse(input).map(FieldAttribute::Inner)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for DeriveAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            DeriveAttribute::Grammar(attr) => attr.to_tokens(tokens),
            DeriveAttribute::Rule(attr) => attr.to_tokens(tokens),
        }
    }
}

impl ToTokens for FieldAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FieldAttribute::Outer(attr) => attr.to_tokens(tokens),
            FieldAttribute::Inner(attr) => attr.to_tokens(tokens),
        }
    }
}

impl Parse for GrammarAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(GrammarAttribute {
            grammar: input.parse()?,
            eq: input.parse()?,
            lit: input.parse()?,
        })
    }
}

impl ToTokens for GrammarAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.grammar.to_tokens(tokens);
        self.eq.to_tokens(tokens);
        self.lit.to_tokens(tokens);
    }
}

impl Parse for OuterAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(OuterAttribute {
            outer: input.parse()?,
            paren: parenthesized!(content in input),
            with: content.parse_terminated(WithAttribute::parse, Token![,])?,
        })
    }
}

impl ToTokens for OuterAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.outer.to_tokens(tokens);
        self.paren.surround(tokens, |tokens| {
            self.with.to_tokens(tokens);
        })
    }
}

impl Parse for InnerAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        let inner = input.parse()?;
        let paren = parenthesized!(content in input);
        let (rule, comma) = if content.peek(kw::rule) {
            (Some(content.parse()?), content.parse().ok())
        } else {
            (None, None)
        };
        let with = content.parse_terminated(WithAttribute::parse, Token![,])?;
        Ok(InnerAttribute {
            inner,
            paren,
            rule,
            comma,
            with,
        })
    }
}

impl ToTokens for InnerAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.inner.to_tokens(tokens);
        self.paren.surround(tokens, |tokens| {
            if let Some(rule) = &self.rule {
                rule.to_tokens(tokens);
            }
            if let Some(comma) = &self.comma {
                comma.to_tokens(tokens);
            }
            self.with.to_tokens(tokens);
        })
    }
}

impl Parse for WithAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(WithAttribute {
            with: input.parse()?,
            paren: parenthesized!(content in input),
            path: content.parse()?,
        })
    }
}

impl ToTokens for WithAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.with.to_tokens(tokens);
        self.paren
            .surround(tokens, |tokens| self.path.to_tokens(tokens));
    }
}

impl Parse for RuleAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        let rule = input.parse()?;
        let paren = parenthesized!(content in input);
        let mut path: Path = content.parse()?;
        let (variant, _) = path.segments.pop().unwrap().into_tuple();
        let sep = if path.segments.trailing_punct() {
            // fix trailing punct
            let (head, sep) = path.segments.pop().unwrap().into_tuple();
            path.segments.push(head);
            sep.unwrap()
        } else {
            Err(Error::new(
                path.span(),
                "must be a path to enum variant (both enum and variant)",
            ))?
        };
        if variant.arguments.is_empty() {
            Ok(RuleAttribute {
                rule,
                paren,
                path,
                sep,
                variant: variant.ident,
            })
        } else {
            Err(Error::new(path.span(), "must be a path to enum variant"))
        }
    }
}

impl ToTokens for RuleAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.rule.to_tokens(tokens);
        self.paren.surround(tokens, |tokens| {
            self.path.to_tokens(tokens);
            self.sep.to_tokens(tokens);
            self.variant.to_tokens(tokens);
        });
    }
}
