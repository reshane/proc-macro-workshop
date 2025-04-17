use proc_macro::TokenStream;
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields, GenericArgument, Meta, PathArguments, Type};
use proc_macro2::{Ident, Literal, TokenTree};
use quote::quote;
use std::ops::{Deref, DerefMut};

#[derive(Clone)]
enum BuilderFieldInfo {
    Basic{
        full: Field,
    },
    Optional{
        full: Field,
        inner_typ: syn::TypePath,
    },
    Extendable{
        full: Field,
        inner_typ: syn::TypePath,
    },
}

impl BuilderFieldInfo {
    fn from_field(src_field: Field) -> Self {
        let field_typ = &src_field.ty;
        let mut inner_typ = None;
        let mut optional = false;
        let mut extendable = false;
        let mut tag = None;
        if let Type::Path(ref typed_path) = field_typ {
            if typed_path.path.segments.iter().any(|segment| { segment.ident.to_string() == "Option".to_string() }) {
                optional = true;
            }
            if typed_path.path.segments.iter().any(|segment| { segment.ident.to_string() == "Vec".to_string() }) {
                extendable = true;
            }
        }
        if optional || extendable {
            if let Type::Path(ref typed_path) = field_typ {
                if let PathArguments::AngleBracketed(ref abga) = typed_path.path.segments[0].arguments {
                    if let GenericArgument::Type(Type::Path(ref inner_typed_path)) = abga.args[0] {
                        inner_typ = Some(inner_typed_path.clone());
                    }
                }
            }
        }

        if optional {
            BuilderFieldInfo::Optional {
                full: src_field.clone(),
                inner_typ: inner_typ.expect("There must be an inner type of an Option<..> field"),
            }
        } else if extendable {
            for attr in src_field.attrs.iter() {
                println!("{:?}", attr);
                if let Meta::List(ref meta_list) = attr.meta {
                    for token in meta_list.tokens.clone().into_iter() {
                        if let TokenTree::Literal(lit) = token {
                            tag = Some(format!("{}", lit));
                            println!("{}", lit);
                        }
                    }
                }
            }
            /*
            let identifier = input.ident;
            let builder_name = format!("{}Builder", identifier);
            let builder_name_ident = Ident::new(builder_name.as_str(), identifier.span());
            */
            BuilderFieldInfo::Extendable {
                full: src_field.clone(),
                inner_typ: inner_typ.expect("There must be an inner type of a Vec<..> field"),
            }
        } else {
            BuilderFieldInfo::Basic {
                full: src_field.clone(),
            }
        }
    }

    fn into_builder_field_def(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldInfo::Basic { full } => {
                let ident = &full.ident;
                let typ = &full.ty;
                quote! { #ident: std::option::Option<#typ> }
            },
            BuilderFieldInfo::Optional { full, inner_typ: _ } => {
                let ident = &full.ident;
                let typ = &full.ty;
                quote! { #ident: #typ }
            },
            BuilderFieldInfo::Extendable { full, inner_typ: _ } => {
                let ident = &full.ident;
                let typ = &full.ty;
                quote! { #ident: #typ }
            },
        }
    }

    fn into_uninitialized_field(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldInfo::Basic { full } => {
                let field = &full.ident;
                quote! { #field: std::option::Option::None }
            },
            BuilderFieldInfo::Optional { full, inner_typ: _ } => {
                let field = &full.ident;
                quote! { #field: std::option::Option::None }
            },
            BuilderFieldInfo::Extendable { full, inner_typ } => {
                let field = &full.ident;
                quote! { #field: std::vec::Vec::<#inner_typ>::new() }
            },
        }
    }

    fn into_builder_method(&self, builder_name_ident: &syn::Ident) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldInfo::Basic { full } => {
                let field_ident = &full.ident;
                let field_typ = &full.ty;
                quote! {
                    pub fn #field_ident(&mut self, #field_ident: #field_typ) -> &mut #builder_name_ident {
                        self.#field_ident = std::option::Option::Some(#field_ident);
                        self
                    }
                }
            },
            BuilderFieldInfo::Optional { full, inner_typ } => {
                let field_ident = &full.ident;
                quote! {
                    pub fn #field_ident(&mut self, #field_ident: #inner_typ) -> &mut #builder_name_ident {
                        self.#field_ident = std::option::Option::Some(#field_ident);
                        self
                    }
                }
            },
            BuilderFieldInfo::Extendable { full, inner_typ } => {
                let field_ident = &full.ident;
                quote! {
                    pub fn #field_ident(&mut self, #field_ident: #inner_typ) -> &mut #builder_name_ident {
                        self.#field_ident.push(#field_ident);
                        self
                    }
                }
            },
        }
    }

    fn into_unwrapping(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldInfo::Basic { full } => {
                let field_ident = &full.ident;
                quote! { #field_ident: self.#field_ident.clone().unwrap() }
            },
            BuilderFieldInfo::Optional { full, inner_typ: _ } => {
                let field_ident = &full.ident;
                quote! { #field_ident: self.#field_ident.clone() }
            },
            BuilderFieldInfo::Extendable { full, inner_typ: _ } => {
                let field_ident = &full.ident;
                quote! { #field_ident: self.#field_ident }
            },
        }
    }

    fn into_validation(&self) -> proc_macro2::TokenStream {
        match self {
            BuilderFieldInfo::Basic { full } => {
                let field_ident = &full.ident;
                quote! {
                    if self.#field_ident.is_none() {
                        return Err(std::boxed::Box::new(
                            format!("Missing field {}", stringify!(#field_ident))
                        ));
                    }
                }
            },
            BuilderFieldInfo::Optional { full: _, inner_typ: _ } => quote!{},
            BuilderFieldInfo::Extendable { full: _, inner_typ: _ } => quote!{},
        }
    }
}

struct BuilderFields(pub Vec<BuilderFieldInfo>);

impl Deref for BuilderFields {
    type Target = Vec<BuilderFieldInfo>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BuilderFields {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let mut full_fields = BuilderFields(vec![]);
    if let Data::Struct(data_struct) = input.data {
        if let Fields::Named(named_fields) = data_struct.fields {
            for named_field in named_fields.named.into_iter() {
                full_fields.push(BuilderFieldInfo::from_field(named_field));
            }
        }
    }

    let identifier = input.ident;
    let builder_name = format!("{}Builder", identifier);
    let builder_name_ident = Ident::new(builder_name.as_str(), identifier.span());

    let uninitialized_fields = full_fields.iter()
        .map(|field| field.into_uninitialized_field() )
        .collect::<Vec<proc_macro2::TokenStream>>();

    // builder() method impl
    let builder_fn = quote! {
        impl #identifier {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #(#uninitialized_fields),*
                }
            }
        }
    };

    // builder struct
    let builder_field_defs = full_fields.clone()
        .into_iter()
        .map(|ff| ff.into_builder_field_def() )
        .collect::<Vec<proc_macro2::TokenStream>>();
    let builder_struct = quote! {
        pub struct #builder_name_ident {
            #(#builder_field_defs),*
        }
    };

    // builder methods
    let builder_methods = full_fields.clone().into_iter()
        .map(|field| field.into_builder_method(&builder_name_ident) )
        .collect::<Vec<proc_macro2::TokenStream>>();

    let final_struct_unwrappings = full_fields.iter()
        .map(|field| field.into_unwrapping() )
        .collect::<Vec<proc_macro2::TokenStream>>();

    let final_struct_validations = full_fields.iter()
        .map(|field| field.into_validation() )
        .collect::<Vec<proc_macro2::TokenStream>>();

    // builder struct impl
    let builder_struct_impl = quote! {
        impl #builder_name_ident {
            #(#builder_methods)*
            pub fn build(&self) -> std::result::Result<#identifier, std::boxed::Box<dyn std::error::Error>> {
                #(#final_struct_validations)*
                Ok(#identifier {
                    #(#final_struct_unwrappings),*
                })
            }
        }
    };

    // expand all of it
    let expanded = quote! {
        #builder_fn
        #builder_struct
        #builder_struct_impl
    };
    
    expanded.into()
}
