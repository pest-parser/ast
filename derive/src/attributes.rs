#![allow(clippy::eval_order_dependence)] // syn patterns

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

pub(crate) enum PestAstAttribute {
    /// grammar = "grammar.rs"
    Grammar(GrammarAttribute),
    /// outer
    Outer(OuterAttribute),
    /// inner
    Inner(InnerAttribute),
    /// with(path::to)
    With(WithAttribute),
    /// rule(path::to)
    Rule(RuleAttribute),
}

pub(crate) struct GrammarAttribute {
    pub(crate) grammar: kw::grammar,
    pub(crate) eq: Token![=],
    pub(crate) lit: LitStr,
}

pub(crate) struct OuterAttribute {
    pub(crate) outer: kw::outer,
}

pub(crate) struct InnerAttribute {
    pub(crate) inner: kw::inner,
}

pub(crate) struct WithAttribute {
    pub(crate) with: kw::with,
    pub(crate) paren: Paren,
    pub(crate) path: Path,
}

pub(crate) struct RuleAttribute {
    pub(crate) rule: kw::rule,
    pub(crate) paren: Paren,
    pub(crate) path: Path,
    pub(crate) sep: Token![::],
    pub(crate) variant: Ident,
}

impl PestAstAttribute {
    pub(crate) fn from_attributes(attrs: impl IntoIterator<Item = Attribute>) -> Result<Vec<Self>> {
        attrs
            .into_iter()
            .map(PestAstAttribute::from_attribute)
            .fold_results(vec![], |mut acc, t| {
                acc.extend(t);
                acc
            })
    }

    pub(crate) fn from_attribute(attr: Attribute) -> Result<Vec<Self>> {
        if attr.path != parse_quote!(pest_ast) {
            return Ok(vec![]);
        }

        Parser::parse2(
            |input: ParseStream| {
                let content;
                parenthesized!(content in input);
                let punctuated: Punctuated<_, Token![,]> =
                    content.parse_terminated(Parse::parse)?;
                Ok(punctuated.into_iter().collect_vec())
            },
            attr.tts,
        )
    }
}

impl Parse for PestAstAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::grammar) {
            GrammarAttribute::parse(input).map(PestAstAttribute::Grammar)
        } else if lookahead.peek(kw::outer) {
            OuterAttribute::parse(input).map(PestAstAttribute::Outer)
        } else if lookahead.peek(kw::inner) {
            InnerAttribute::parse(input).map(PestAstAttribute::Inner)
        } else if lookahead.peek(kw::with) {
            WithAttribute::parse(input).map(PestAstAttribute::With)
        } else if lookahead.peek(kw::rule) {
            RuleAttribute::parse(input).map(PestAstAttribute::Rule)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for PestAstAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            PestAstAttribute::Grammar(attr) => attr.to_tokens(tokens),
            PestAstAttribute::Outer(attr) => attr.to_tokens(tokens),
            PestAstAttribute::Inner(attr) => attr.to_tokens(tokens),
            PestAstAttribute::With(attr) => attr.to_tokens(tokens),
            PestAstAttribute::Rule(attr) => attr.to_tokens(tokens),
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
        Ok(OuterAttribute {
            outer: input.parse()?,
        })
    }
}

impl ToTokens for OuterAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.outer.to_tokens(tokens);
    }
}

impl Parse for InnerAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(InnerAttribute {
            inner: input.parse()?,
        })
    }
}

impl ToTokens for InnerAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.inner.to_tokens(tokens);
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
