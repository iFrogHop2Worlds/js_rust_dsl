extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Lit, Token};

struct JsScript {
    statements: Vec<JsStatement>,
}

enum JsStatement {
    LetDecl(Ident, JsExpression),
    ConsoleLog(JsExpression),
    IfStatement(JsExpression, JsBlock),
}

enum JsExpression {
    Literal(JsLiteral),
    Identifier(Ident),
    BinaryOp(Box<JsExpression>, JsBinaryOp, Box<JsExpression>),
}

enum JsBinaryOp {
    Add,
}

enum JsLiteral {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

struct JsBlock {
    statements: Vec<JsStatement>,
}

impl Parse for JsScript {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut statements = Vec::new();
        while !input.is_empty() {
            statements.push(input.parse()?);
            if input.peek(Token![;]) {
                let _ = input.parse::<Token![;]>();
            }
        }
        Ok(JsScript { statements })
    }
}

impl Parse for JsStatement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![let]) {
            let _ = input.parse::<Token![let]>()?;
            let var_name: Ident = input.parse()?;
            let _ = input.parse::<Token![=]>()?;
            let value: JsExpression = input.parse()?;
            Ok(JsStatement::LetDecl(var_name, value))
        } else if lookahead.peek(Ident) {
            let fork = input.fork();
            let ident1: Ident = fork.parse()?;
            if ident1 == "console" && fork.peek(Token![.]) {
                fork.parse::<Token![.]>()?;
                if fork.peek(Ident) {
                    let ident2: Ident = fork.parse()?;
                    if ident2 == "log" {
                        let _ = input.parse::<Ident>()?;
                        let _ = input.parse::<Token![.]>()?;
                        let _ = input.parse::<Ident>()?;
                        let content;
                        syn::parenthesized!(content in input);
                        let expr: JsExpression = content.parse()?;
                        return Ok(JsStatement::ConsoleLog(expr));
                    }
                }
            }
            Err(input.error("expected 'let', 'console.log', or 'if' statement"))
        } else if lookahead.peek(Token![if]) {
            let _ = input.parse::<Token![if]>()?;
            let condition_content;
            syn::parenthesized!(condition_content in input);
            let condition_expr: JsExpression = condition_content.parse()?;
            let block: JsBlock = input.parse()?;
            Ok(JsStatement::IfStatement(condition_expr, block))
        } else {
            Err(lookahead.error())
        }
    }
}

/// Parses a "primary" expression (a single literal or identifier).
fn parse_primary_expression(input: ParseStream) -> syn::Result<JsExpression> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Lit) {
        let lit: Lit = input.parse()?;
        Ok(JsExpression::Literal(lit.try_into()?))
    } else if lookahead.peek(Ident) {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "null" => Ok(JsExpression::Literal(JsLiteral::Null)),
            "undefined" => Ok(JsExpression::Literal(JsLiteral::Undefined)),
            _ => Ok(JsExpression::Identifier(ident)),
        }
    } else {
        Err(lookahead.error())
    }
}

impl Parse for JsExpression {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lhs = parse_primary_expression(input)?;
        
        while input.peek(Token![+]) {
            let op = if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                JsBinaryOp::Add
            } else {
                // will not be reached if only `+` is checked
                // for future expansion with other operators.
                break;
            };

            let rhs = parse_primary_expression(input)?;
            lhs = JsExpression::BinaryOp(Box::new(lhs), op, Box::new(rhs));
        }

        Ok(lhs)
    }
}

impl TryFrom<Lit> for JsLiteral {
    type Error = syn::Error;

    fn try_from(lit: Lit) -> syn::Result<Self> {
        match lit {
            Lit::Int(int_lit) => Ok(JsLiteral::Number(int_lit.base10_parse::<f64>()?)),
            Lit::Float(float_lit) => Ok(JsLiteral::Number(float_lit.base10_parse::<f64>()?)),
            Lit::Str(str_lit) => Ok(JsLiteral::String(str_lit.value())),
            Lit::Bool(bool_lit) => Ok(JsLiteral::Boolean(bool_lit.value())),
            _ => Err(syn::Error::new_spanned(
                lit,
                "unsupported literal type for JsValue",
            )),
        }
    }
}

impl Parse for JsBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::braced!(content in input);
        let mut statements = Vec::new();
        while !content.is_empty() {
            statements.push(content.parse()?);
            if content.peek(Token![;]) {
                let _ = content.parse::<Token![;]>();
            }
        }
        Ok(JsBlock { statements })
    }
}

// ~~~ Code Gen ~~~

impl ToTokens for JsScript {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for statement in &self.statements {
            statement.to_tokens(tokens);
        }
    }
}

impl ToTokens for JsStatement {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            JsStatement::LetDecl(var_name, value_expr) => {
                tokens.extend(quote! {
                    let #var_name: js_runtime::JsValue = #value_expr;
                });
            }
            JsStatement::ConsoleLog(expr) => {
                tokens.extend(quote! {
                    js_runtime::console_log(&#expr);
                });
            }
            JsStatement::IfStatement(condition_expr, block) => {
                tokens.extend(quote! {
                    if #condition_expr.to_bool() {
                        #block
                    }
                });
            }
        }
    }
}

impl ToTokens for JsExpression {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            JsExpression::Literal(lit) => lit.to_tokens(tokens),
            JsExpression::Identifier(ident) => {
                tokens.extend(quote! { #ident.clone() });
            }
            JsExpression::BinaryOp(lhs, op, rhs) => {
                let op_token = match op {
                    JsBinaryOp::Add => quote! { .add },
                };
                tokens.extend(quote! {
                    (#lhs)#op_token(&#rhs)
                });
            }
        }
    }
}

impl ToTokens for JsLiteral {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            JsLiteral::Number(n) => tokens.extend(quote! { js_runtime::JsValue::Number(#n) }),
            JsLiteral::String(s) => tokens.extend(quote! { js_runtime::JsValue::String(#s.to_string()) }),
            JsLiteral::Boolean(b) => tokens.extend(quote! { js_runtime::JsValue::Boolean(#b) }),
            JsLiteral::Null => tokens.extend(quote! { js_runtime::JsValue::Null }),
            JsLiteral::Undefined => tokens.extend(quote! { js_runtime::JsValue::Undefined }),
        }
    }
}

impl ToTokens for JsBlock {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let statements = &self.statements;
        tokens.extend(quote! {
            {
                #(#statements)*
            }
        });
    }
}

// ~~~ Macro Entry Point ~~~
#[proc_macro]
pub fn js_script(input: TokenStream) -> TokenStream {
    let script = syn::parse_macro_input!(input as JsScript);
    let expanded = quote! { #script };
    expanded.into()
}