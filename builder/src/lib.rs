use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, DataStruct, DeriveInput, Field, Fields, FieldsNamed,
    GenericArgument, Ident, Lit, PathArguments, PathSegment, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let builder_name = format_ident!("{}Builder", &ast.ident, span=ast.ident.span());
    let named_fields = match ast.data {
        syn::Data::Struct(DataStruct {
            fields:
                Fields::Named(FieldsNamed {
                    named: named_fields,
                    ..
                }),
            ..
        }) => named_fields,
        _ => {
            return quote_spanned!(ast.ident.span() => compile_error!("wants struct with named fields")).into();
        }
    };

    let mut struct_fields = vec![];
    let mut constructor_fields = vec![];
    let mut call_setters = vec![];
    let mut builder_fields = vec![];
    named_fields
        .iter()
        .map(generate_code_components_for_field)
        .for_each(
            |[struct_field, constructor_field, call_setter, builder_field]| {
                struct_fields.push(struct_field);
                constructor_fields.push(constructor_field);
                builder_fields.push(builder_field);
                call_setters.push(call_setter);
            },
        );

    quote!(
        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#constructor_fields ,)*
                }
            }
        }
        struct #builder_name {
            #(#struct_fields ,)*
        }
        impl #builder_name {
            #(#call_setters)*
            pub fn build(&self) -> ::std::result::Result<#name, std::boxed::Box<dyn ::std::error::Error>> {
                std::result::Result::Ok(#name { #(#builder_fields ,)* })
            }
        }
    )
    .into()
}

fn extract_inner_of_path<'a>(ty: &'a syn::Type, expected_paths: &[&str]) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref type_path) = ty {
        if expected_paths.iter().any(|path| {
            type_path
                .path
                .segments
                .iter()
                .zip(path.split("::"))
                .all(|(a, b)| a.ident == b)
        }) {
            if let Some(PathSegment {
                arguments: PathArguments::AngleBracketed(ref last),
                ..
            }) = type_path.path.segments.last()
            {
                if let Some(GenericArgument::Type(ref ty)) = last.args.first() {
                    return Some(ty);
                }
            }
        }
    }
    None
}

enum FieldType {
    Normal(proc_macro2::TokenStream),
    EachVector { ty: Type, name: Ident },
    Option(Type),
}

impl std::convert::From<&Field> for FieldType {
    fn from(field: &Field) -> Self {
        if let Some(attr) = field.attrs.iter().find(|a| a.path.is_ident("builder")) {
            if let Ok(syn::Meta::NameValue(each)) = attr.parse_args() {
                let option_ident = each.path.get_ident();
                if option_ident.is_some() && option_ident.unwrap() == "each" {
                    if let Lit::Str(ref name) = each.lit {
                        let field_name = format_ident!("{}", name.value(), span = each.lit.span());
                        if let Some(ref ty) = extract_inner_of_path(
                            &field.ty,
                            &["alloc::vec::Vec", "std::vec::Vec", "Vec"],
                        ) {
                            return Self::EachVector {
                                ty: Clone::clone(*ty),
                                name: field_name,
                            };
                        } else {
                            return Self::Normal(quote_spanned!(
                            field.ty.span() =>
                                compile_error!("`builder` attribute expects `Vec`")
                            ));
                        };
                    }
                }
            }
            return Self::Normal(
                syn::Error::new_spanned(
                    attr.parse_meta().unwrap(),
                    "expected `builder(each = \"...\")`",
                )
                .to_compile_error(),
            );
        } else if let Some(ty) = extract_inner_of_path(
            &field.ty,
            &["std::option::Option", "core::option::Option", "Option"],
        ) {
            return Self::Option(ty.clone());
        } else {
            return Self::Normal(quote!());
        }
    }
}

fn generate_code_components_for_field(field: &Field) -> [proc_macro2::TokenStream; 4] {
    let typed_field = field.into();
    let field_name = field.ident.as_ref().unwrap();
    // TODO: .ok_or(|| quote_spanned!(field.span() => compile_error!("expected named field")).into())?;

    let span = field_name.span();
    let field_type = &field.ty;

    let struct_field = if matches!(typed_field, FieldType::Normal(_)) {
        quote_spanned! { span => #field_name: ::std::option::Option<#field_type> }
    } else {
        quote_spanned! { span => #field_name: #field_type }
    };
    let constructor_field =
        quote_spanned! { span => #field_name: ::std::default::Default::default() };

    let builder_field = if matches!(typed_field, FieldType::Normal(_)) {
        quote_spanned! { span =>
            #field_name: self.#field_name.clone().ok_or(concat!(stringify!(#field_name), " is not set"))?
        }
    } else {
        quote_spanned! { span => #field_name: self.#field_name.clone() }
    };

    let call_setter = match typed_field {
        FieldType::Normal(err) => quote_spanned! { span =>
            pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                self.#field_name = ::std::option::Option::Some(#field_name);
                self
            }
            #err
        },
        FieldType::EachVector { ty, name } if name.to_string() != field_name.to_string() => {
            quote_spanned! { span =>
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#field_name.push(#name);
                    self
                }
                pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = #field_name;
                    self
                }
            }
        }
        FieldType::EachVector { ty, name } => quote_spanned! { span =>
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#field_name.push(#name);
                self
            }
        },
        FieldType::Option(ty) => quote_spanned! { span =>
            pub fn #field_name(&mut self, #field_name: #ty) -> &mut Self {
                self.#field_name = ::std::option::Option::Some(#field_name);
                self
            }
        },
    };
    [struct_field, constructor_field, call_setter, builder_field]
}
