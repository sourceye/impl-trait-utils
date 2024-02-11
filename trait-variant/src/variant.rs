// Copyright (c) 2023 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::iter;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::{Comma, Plus},
    Error, FnArg, GenericParam, Ident, ItemTrait, Pat, PatType, Result, ReturnType, Signature,
    Token, TraitBound, TraitItem, TraitItemConst, TraitItemFn, TraitItemType, Type, TypeGenerics,
    TypeImplTrait, TypeParam, TypeParamBound,
};

struct Attrs {
    variant: MakeVariant,
    replacements: Vec<Replacement>,
}
impl From<RawAttrs> for Attrs {
    fn from(value: RawAttrs) -> Self {
        let mut variant = None;
        let mut replacements = Vec::new();
        for attr in value.attrs {
            match attr {
                Attr::MakeVariant(v) => variant = Some(v),
                Attr::Replacement(r) => replacements.push(r),
            }
        }
        let Some(variant) = variant else {
            panic!("There should be a variant")
        };
        Self {
            variant,
            replacements,
        }
    }
}

struct RawAttrs {
    attrs: Punctuated<Attr, Comma>,
}

impl Parse for RawAttrs {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            attrs: input.parse_terminated(Attr::parse, Token![,])?,
        })
    }
}

enum Attr {
    MakeVariant(MakeVariant),
    Replacement(Replacement),
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(if input.peek2(Token![:]) {
            Attr::MakeVariant(input.parse()?)
        } else {
            Attr::Replacement(input.parse()?)
        })
    }
}

struct MakeVariant {
    name: Ident,
    #[allow(unused)]
    colon: Token![:],
    bounds: Punctuated<TraitBound, Plus>,
}

impl Parse for MakeVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            name: input.parse()?,
            colon: input.parse()?,
            bounds: Punctuated::parse_separated_nonempty(input)?,
        })
    }
}

struct Replacement {
    original: Ident,
    #[allow(unused)]
    aka: Token![as],
    variant_name: Ident,
}

impl Parse for Replacement {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            original: input.parse()?,
            aka: input.parse()?,
            variant_name: input.parse()?,
        })
    }
}

pub fn make(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: Attrs = parse_macro_input!(attr as RawAttrs).into();
    let item = parse_macro_input!(item as ItemTrait);

    let maybe_allow_async_lint = if attrs
        .variant
        .bounds
        .iter()
        .any(|b| b.path.segments.last().unwrap().ident == "Send")
    {
        quote! { #[allow(async_fn_in_trait)] }
    } else {
        quote! {}
    };

    let variant = mk_variant(&attrs, &item);
    let blanket_impl = mk_blanket_impl(&attrs, &item);

    quote! {
        #maybe_allow_async_lint
        #item

        #variant

        #blanket_impl
    }
    .into()
}

fn mk_variant(attrs: &Attrs, tr: &ItemTrait) -> TokenStream {
    let MakeVariant {
        ref name,
        colon: _,
        ref bounds,
    } = attrs.variant;
    let bounds: Vec<_> = bounds
        .into_iter()
        .map(|b| TypeParamBound::Trait(b.clone()))
        .collect();
    let variant = ItemTrait {
        ident: name.clone(),
        supertraits: tr.supertraits.iter().chain(&bounds).cloned().collect(),
        items: tr
            .items
            .iter()
            .map(|item| transform_item(item, &bounds, &attrs.replacements))
            .collect(),
        ..tr.clone()
    };
    quote! { #variant }
}

fn transform_item(
    item: &TraitItem,
    bounds: &Vec<TypeParamBound>,
    replacements: &Vec<Replacement>,
) -> TraitItem {
    // #[make_variant(SendIntFactory: Send)]
    // trait IntFactory {
    //     async fn make(&self, x: u32, y: &str) -> i32;
    //     fn stream(&self) -> impl Iterator<Item = i32>;
    //     fn call(&self) -> u32;
    // }
    //
    // becomes:
    //
    // trait SendIntFactory: Send {
    //     fn make(&self, x: u32, y: &str) -> impl ::core::future::Future<Output = i32> + Send;
    //     fn stream(&self) -> impl Iterator<Item = i32> + Send;
    //     fn call(&self) -> u32;
    // }
    let TraitItem::Fn(fn_item @ TraitItemFn { sig, .. }) = item else {
        let TraitItem::Type(trait_type) = item else {
            return item.clone();
        };
        return TraitItem::Type(TraitItemType {
            attrs: trait_type.attrs.clone(),
            type_token: trait_type.type_token.clone(),
            ident: trait_type.ident.clone(),
            generics: trait_type.generics.clone(),
            colon_token: trait_type.colon_token.clone(),
            bounds: update_bounds(&trait_type.bounds, replacements),
            default: trait_type.default.clone(),
            semi_token: trait_type.semi_token.clone(),
        });
    };
    let (arrow, output) = if sig.asyncness.is_some() {
        let orig = match &sig.output {
            ReturnType::Default => quote! { () },
            ReturnType::Type(_, ty) => quote! { #ty },
        };
        let future = syn::parse2(quote! { ::core::future::Future<Output = #orig> }).unwrap();
        let ty = Type::ImplTrait(TypeImplTrait {
            impl_token: syn::parse2(quote! { impl }).unwrap(),
            bounds: iter::once(TypeParamBound::Trait(future))
                .chain(bounds.iter().cloned())
                .collect(),
        });
        (syn::parse2(quote! { -> }).unwrap(), ty)
    } else {
        match &sig.output {
            ReturnType::Type(arrow, ty) => match &**ty {
                Type::ImplTrait(it) => {
                    let ty = Type::ImplTrait(TypeImplTrait {
                        impl_token: it.impl_token,
                        bounds: it.bounds.iter().chain(bounds).cloned().collect(),
                    });
                    (*arrow, ty)
                }
                _ => return item.clone(),
            },
            ReturnType::Default => return item.clone(),
        }
    };
    TraitItem::Fn(TraitItemFn {
        sig: Signature {
            asyncness: None,
            output: ReturnType::Type(arrow, Box::new(output)),
            ..sig.clone()
        },
        ..fn_item.clone()
    })
}

fn update_bounds(
    bounds: &Punctuated<TypeParamBound, Plus>,
    replacements: &Vec<Replacement>,
) -> Punctuated<TypeParamBound, Plus> {
    let mut out = Punctuated::new();
    for el in bounds {
        let TypeParamBound::Trait(bound) = el else {
            out.push(el.clone());
            continue;
        };
        let mut bound = bound.clone();
        let to_replace = bound.path.segments.last_mut().unwrap();
        for rep in replacements {
            if to_replace.ident == rep.original {
                to_replace.ident = rep.variant_name.clone();
            }
        }
        out.push(TypeParamBound::Trait(bound))
    }
    out
}

fn mk_blanket_impl(attrs: &Attrs, tr: &ItemTrait) -> TokenStream {
    let orig = &tr.ident;
    let variant = &attrs.variant.name;
    let (_impl, orig_ty_generics, _where) = &tr.generics.split_for_impl();
    let items = tr
        .items
        .iter()
        .map(|item| blanket_impl_item(item, variant, orig_ty_generics));
    let blanket_bound: TypeParam =
        parse_quote!(TraitVariantBlanketType: #variant #orig_ty_generics);
    let blanket = &blanket_bound.ident.clone();
    let mut blanket_generics = tr.generics.clone();
    blanket_generics
        .params
        .push(GenericParam::Type(blanket_bound));
    let (blanket_impl_generics, _ty, blanket_where_clause) = &blanket_generics.split_for_impl();
    quote! {
        impl #blanket_impl_generics #orig #orig_ty_generics for #blanket #blanket_where_clause
        {
            #(#items)*
        }
    }
}

fn blanket_impl_item(
    item: &TraitItem,
    variant: &Ident,
    trait_ty_generics: &TypeGenerics<'_>,
) -> TokenStream {
    // impl<T> IntFactory for T where T: SendIntFactory {
    //     const NAME: &'static str = <Self as SendIntFactory>::NAME;
    //     type MyFut<'a> = <Self as SendIntFactory>::MyFut<'a> where Self: 'a;
    //     async fn make(&self, x: u32, y: &str) -> i32 {
    //         <Self as SendIntFactory>::make(self, x, y).await
    //     }
    // }
    match item {
        TraitItem::Const(TraitItemConst {
            ident,
            generics,
            ty,
            ..
        }) => {
            quote! {
                const #ident #generics: #ty = <Self as #variant #trait_ty_generics>::#ident;
            }
        }
        TraitItem::Fn(TraitItemFn { sig, .. }) => {
            let ident = &sig.ident;
            let args = sig.inputs.iter().map(|arg| match arg {
                FnArg::Receiver(_) => quote! { self },
                FnArg::Typed(PatType { pat, .. }) => match &**pat {
                    Pat::Ident(arg) => quote! { #arg },
                    _ => Error::new_spanned(pat, "patterns are not supported in arguments")
                        .to_compile_error(),
                },
            });
            let maybe_await = if sig.asyncness.is_some() {
                quote! { .await }
            } else {
                quote! {}
            };
            quote! {
                #sig {
                    <Self as #variant #trait_ty_generics>::#ident(#(#args),*)#maybe_await
                }
            }
        }
        TraitItem::Type(TraitItemType {
            ident, generics, ..
        }) => {
            let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
            quote! {
                type #ident #impl_generics = <Self as #variant #trait_ty_generics>::#ident #ty_generics #where_clause;
            }
        }
        _ => Error::new_spanned(item, "unsupported item type").into_compile_error(),
    }
}
