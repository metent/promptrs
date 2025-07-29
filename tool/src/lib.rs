//! Procedural macro for defining structured tool functions.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use std::collections::HashMap;
use syn::parse_macro_input;

/// # `#[tool]` Attribute Macro
///
/// Generates a `Tool` implementation for annotated functions:
///
/// 1. Creates struct wrapper with function
/// 2. Implements `Tool` trait with:
///    - `name()`: Function name
///    - `arguments()`: JSON schema from doc comments
///    - `call()`: Type-safe argument parsing
///
/// ## Example
///
/// ```rust
/// use promptrs::{tool, Tooling};
///
/// #[tool]
/// /// Calculator tool
/// /// a: First number
/// /// b: Second number
/// /// operation: +, -, *, /
/// fn calculate(a: f64, b: f64, operation: String) -> String {
///     match operation.as_str() {
///         "+" => (a + b).to_string(),
///         "-" => (a - b).to_string(),
///         "*" => (a * b).to_string(),
///         "/" => a / b > 0.0 ? (a / b).to_string() : "Division by zero".into(),
///         _ => "Invalid operation".into(),
///     }
/// }
///
/// let tooling = Tooling::new().add(calculate);
/// let args = Arguments::from_value(json!({
///     "a": 5.5,
///     "b": 2.5,
///     "operation": "*"
/// }));
///
/// let result = tooling.call("calculate", &args).unwrap();
/// println!("Result: {}", result); // 13.75
/// ```
///
/// ## Documentation Comments
///
/// Use `/// name: Description` to:
/// 1. Create parameter documentation in JSON schema
/// 2. Provide user-facing parameter descriptions
/// 3. Enable parameter auto-completion in development tools
#[proc_macro_attribute]
pub fn tool(_attr: TokenStream, item: TokenStream) -> TokenStream {
	let input = parse_macro_input!(item as syn::ItemFn);
	let doc_comments = input.attrs.iter().filter_map(|attr| {
		let meta = attr.meta.require_name_value().ok()?;
		meta.path.is_ident("doc").then_some(())?;
		let syn::Expr::Lit(expr_lit) = &meta.value else {
			return None;
		};
		let syn::Lit::Str(lit_str) = &expr_lit.lit else {
			return None;
		};
		Some(lit_str.value())
	});
	let mut pycomments = String::new();
	let mut prev = None;
	let mut description = String::new();
	let mut attr_map = HashMap::<String, String>::new();
	for mut line in doc_comments {
		pycomments.push_str("#");
		pycomments.push_str(&line);
		pycomments.push_str("\n");
		line = line.split_off(line.find(line.trim_start()).unwrap_or(0));
		if let Some(i) = line.find(':') {
			if let Some((param, desc)) = prev {
				attr_map.insert(param, desc);
			}

			let mut desc = line.split_off(i);
			desc = desc.split_off(1);
			desc = desc.split_off(desc.find(desc.trim_start()).unwrap_or(0));

			prev = Some((line, desc));
		} else if let Some((_, desc)) = &mut prev {
			desc.push('\n');
			desc.push_str(&line);
		} else {
			description.push('\n');
			description.push_str(&line);
		}
	}
	if let Some((param, desc)) = prev {
		attr_map.insert(param, desc);
	}

	let mut params = Vec::new();
	let mut pyparams = Vec::new();
	let mut required_params = Vec::new();
	let mut args = Vec::new();
	let mut call_stmts = Vec::new();
	let mut state_arg = quote!();
	let mut state_ty = quote!();
	let (mut i, mut j) = (0, 0);
	for arg in &input.sig.inputs {
		if let syn::FnArg::Typed(syn::PatType { pat, ty, .. }) = arg {
			if let syn::Type::Reference(syn::TypeReference { elem, .. }) = &**ty {
				let ident = syn::Ident::new("state", Span::call_site());
				state_arg = quote!(#ident : #ty,);
				state_ty = quote!(type State = #elem;);
				args.push(ident);
				continue;
			}
			if let syn::Pat::Ident(pat_ident) = &**pat {
				let ident = &pat_ident.ident;
				let name = ident.to_string();
				let name_ref = &name;
				let index = if name.starts_with("_") {
					j += 1;
					(j - 1).to_string() + "_"
				} else {
					i += 1;
					(i - 1).to_string()
				};
				let desc = attr_map.get(&name).cloned().unwrap_or_default();
				let (schema, required) = jsonschema(ty.clone(), desc);
				let pyschema = pythonschema(&ty);

				args.push(ident.clone());
				if required {
					call_stmts.push(quote! {
						let #ident : #ty = ::promptrs::serde_json::from_value(
							arguments.remove(#name_ref).or_else(|| arguments.remove(#index)).ok_or(::promptrs::serde::de::Error::missing_field(#name_ref))?
						)?;
					});
				} else {
					call_stmts.push(quote! {
						let #ident : #ty = ::promptrs::serde_json::from_value(
							arguments.remove(#name_ref).or_else(|| arguments.remove(#index)).unwrap_or(::promptrs::serde_json::Value::Null)
						)?;
					});
				}
				if name.starts_with('_') {
					continue;
				}

				params.push(format!(r#""{}": {}"#, name, schema));
				pyparams.push(format!("{}: {}", name, pyschema));
				if required {
					required_params.push(name);
				}
			}
		}
	}
	let arguments = format!(
		r#"{{ "description": "{}", "properties": {{ {} }}, "required": ["{}"] }}"#,
		description,
		params.join(", "),
		required_params.join(r#"", ""#)
	);

	let syn::ItemFn {
		vis,
		sig: syn::Signature {
			ident,
			inputs,
			output,
			generics,
			..
		},
		block,
		..
	} = input;
	let name = ident.to_string();
	let pydef = format!("{}def {}({})", pycomments, name, pyparams.join(", "));
	let output = quote! {
		#[allow(non_camel_case_types)]
		struct #ident;

		impl #ident {
			#vis fn run #generics (#inputs) #output {
				#block
			}
		}

		impl ::promptrs::Tool for #ident {
			#state_ty

			fn name(&self) -> &str {
				#name
			}

			fn arguments(&self) -> &str {
				#arguments
			}

			fn pydef(&self) -> &str {
				#pydef
			}

			fn call(&self, #state_arg arguments: &::promptrs::Arguments) -> ::promptrs::serde_json::Result<String> {
				let mut arguments = arguments.clone();
				#(#call_stmts)*
				let result = #ident::run(#(#args,)*);
				Ok(::std::string::ToString::to_string(&result))
			}
		}
	};

	output.into()
}

fn jsonschema(mut ty: Box<syn::Type>, description: String) -> (String, bool) {
	if let syn::Type::Reference(syn::TypeReference { elem, .. }) = &*ty {
		ty = elem.clone();
	}
	let syn::Type::Path(syn::TypePath { path, .. }) = &*ty else {
		return ("unknown".into(), true);
	};
	if let Some(syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
		args,
		..
	})) = path.segments.last().and_then(|l| {
		l.ident
			.to_string()
			.as_str()
			.eq("Option")
			.then(|| &l.arguments)
	}) {
		if let Some(syn::GenericArgument::Type(ty)) = args.first() {
			return (
				format!(
					r#"{}, "description": "{}" }}"#,
					_jsonschema(ty),
					description
				),
				false,
			);
		}
	}
	(
		format!(
			r#"{}, "description": "{}" }}"#,
			_jsonschema(&ty),
			description
		),
		true,
	)
}

fn _jsonschema(ty: &syn::Type) -> String {
	match ty {
		syn::Type::Array(syn::TypeArray { elem, .. }) => {
			format!(r#"{{ "type": "array", "items": {} }}"#, _jsonschema(&elem))
		}
		syn::Type::Reference(syn::TypeReference { elem, .. }) => _jsonschema(&elem),
		syn::Type::Slice(syn::TypeSlice { elem, .. }) => {
			let inner = _jsonschema(&elem);
			if &inner == "str" {
				return "string".into();
			}
			format!(r#"{{ "type": "array", "items": {} }}"#, inner)
		}
		syn::Type::Path(syn::TypePath { path, .. }) => {
			let (ident, arguments) = match path.segments.last() {
				Some(syn::PathSegment {
					ident, arguments, ..
				}) => (ident.into_token_stream().to_string(), arguments),
				None => return "".into(),
			};
			let ty = match ident.as_str() {
				"String" | "str" | "char" => "string".into(),
				"f32" | "f64" => "number".into(),
				"u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" => "integer".into(),
				"bool" => "boolean".into(),
				"Vec" => {
					let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
						args,
						..
					}) = arguments
					else {
						return "".into();
					};
					let Some(syn::GenericArgument::Type(ty)) = args.first() else {
						return "".into();
					};
					format!(r#"{{ "type": "array", "items": {} }}"#, _jsonschema(&ty))
				}
				_ => "".into(),
			};
			format!(r#"{{ "type": "{}""#, ty)
		}
		_ => "unknown".into(),
	}
}

fn pythonschema(ty: &syn::Type) -> String {
	match ty {
		syn::Type::Array(syn::TypeArray { elem, .. }) => {
			format!("list[{}]", pythonschema(&elem))
		}
		syn::Type::Reference(syn::TypeReference { elem, .. }) => pythonschema(&elem),
		syn::Type::Slice(syn::TypeSlice { elem, .. }) => {
			let inner = pythonschema(&elem);
			if &inner == "str" {
				return "str".into();
			}
			format!("list[{}]", pythonschema(&elem))
		}
		syn::Type::Path(syn::TypePath { path, .. }) => {
			let (ident, arguments) = match path.segments.last() {
				Some(syn::PathSegment {
					ident, arguments, ..
				}) => (ident.into_token_stream().to_string(), arguments),
				None => return "".into(),
			};
			match ident.as_str() {
				"String" | "str" | "char" => "str".into(),
				"f32" | "f64" => "float".into(),
				"u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" => "int".into(),
				"bool" => "bool".into(),
				"Option" => {
					let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
						args,
						..
					}) = arguments
					else {
						return "".into();
					};
					let Some(syn::GenericArgument::Type(ty)) = args.first() else {
						return "".into();
					};
					format!("Optional[{}]", pythonschema(ty))
				}
				"Vec" => {
					let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
						args,
						..
					}) = arguments
					else {
						return "".into();
					};
					let Some(syn::GenericArgument::Type(ty)) = args.first() else {
						return "".into();
					};
					format!("list[{}]", pythonschema(ty))
				}
				_ => "".into(),
			}
		}
		_ => "unknown".into(),
	}
}
