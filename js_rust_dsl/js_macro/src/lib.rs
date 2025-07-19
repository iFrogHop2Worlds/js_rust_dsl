extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{
    braced, parenthesized, parse_macro_input, token, Ident, Lit, LitBool, LitFloat, LitStr, Token,
};

mod parser {
    use super::*;
    use proc_macro2::TokenStream as TokenStream2;
    use syn::custom_keyword;

    custom_keyword!(function);
    custom_keyword!(_const);

    pub struct JsScript {
        statements: Vec<JsStatement>,
    }

    #[derive(Clone)]
    pub enum JsStatement {
        LetDecl(Ident, JsExpression),
        IfStatement(JsExpression, JsBlock),
        FunctionDecl(JsFunctionDecl),
        ForLoop(JsForLoop),
        WhileLoop(JsWhileLoop),
        DoWhileLoop(JsDoWhileLoop),
        Expression(JsExpression),
    }

    #[derive(Clone)]
    pub enum JsExpression {
        Literal(JsLiteral),
        Identifier(Ident),
        BinaryOp(Box<JsExpression>, JsBinaryOp, Box<JsExpression>),
        Assignment(Ident, Box<JsExpression>),
        Call(Ident, Vec<JsExpression>),
        Array(Vec<JsExpression>),
        Object(JsObject),
        MemberAccess(Box<JsExpression>, Ident),
        MethodCall {
            object: Box<JsExpression>,
            method: Ident,
            args: Vec<JsExpression>,
        },
    }

    #[derive(Clone)]
    pub struct JsObject {
        properties: Punctuated<JsProperty, Token![,]>,
    }

    #[derive(Clone)]
    pub struct JsProperty {
        key: Ident,
        value: JsExpression,
    }

    #[derive(Clone)]
    pub enum JsBinaryOp {
        Add,
        Equal,
        NotEqual,
        LessThan,
        GreaterThan,
        LessThanOrEqual,
        GreaterThanOrEqual,
    }

    #[derive(Clone)]
    pub enum JsLiteral {
        Number(f64),
        String(String),
        Boolean(bool),
        Null,
        Undefined,
        Function(JsFunctionDecl),
    }

    #[derive(Clone)]
    pub struct JsBlock {
        statements: Vec<JsStatement>,
    }

    #[derive(Clone)]
    pub struct JsFunctionDecl {
        // Anonymous functions won't have a name
        name: Option<Ident>,
        params: Vec<Ident>,
        body: JsBlock,
    }

    #[derive(Clone)]
    pub struct JsWhileLoop {
        condition: JsExpression,
        body: JsBlock,
    }

    #[derive(Clone)]
    pub struct JsDoWhileLoop {
        body: JsBlock,
        condition: JsExpression,
    }

    #[derive(Clone)]
    pub struct JsForLoop {
        init: Option<Box<JsStatement>>,
        condition: Option<JsExpression>,
        increment: Option<Box<JsExpression>>,
        body: JsBlock,
    }

    impl Parse for JsScript {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut statements = Vec::new();
            while !input.is_empty() {
                statements.push(input.parse()?);
            }
            Ok(JsScript { statements })
        }
    }

    impl Parse for JsStatement {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![let]) || lookahead.peek(self::_const) {
                if input.peek(Token![let]) {
                    input.parse::<Token![let]>()?;
                } else {
                    input.parse::<self::_const>()?;
                }
                let id: Ident = input.parse()?;
                input.parse::<Token![=]>()?;
                let expr: JsExpression = input.parse()?;
                input.parse::<Token![;]>()?;
                Ok(JsStatement::LetDecl(id, expr))
            } else if lookahead.peek(Token![if]) {
                input.parse::<Token![if]>()?;
                let content;
                parenthesized!(content in input);
                let cond = content.parse()?;
                let body = input.parse()?;
                Ok(JsStatement::IfStatement(cond, body))
            } else if lookahead.peek(function) {
                Ok(JsStatement::FunctionDecl(input.parse()?))
            } else if lookahead.peek(Token![for]) {
                Ok(JsStatement::ForLoop(input.parse()?))
            } else if lookahead.peek(Token![while]) {
                Ok(JsStatement::WhileLoop(input.parse()?))
            } else if lookahead.peek(Token![do]) {
                Ok(JsStatement::DoWhileLoop(input.parse()?))
            } else {
                let expr = input.parse()?;
                input.parse::<Token![;]>()?;
                Ok(JsStatement::Expression(expr))
            }
        }
    }


    fn parse_primary_expression(input: ParseStream) -> Result<JsExpression> {
        if input.peek(function) {
            let func: JsFunctionDecl = input.parse()?;
            return Ok(JsExpression::Literal(JsLiteral::Function(func)));
        }

        let lookahead = input.lookahead1();
        if lookahead.peek(Lit) {
            let lit: Lit = input.parse()?;
            Ok(JsExpression::Literal(JsLiteral::try_from(lit)?))
        } else if lookahead.peek(Ident) {
            Ok(JsExpression::Identifier(input.parse()?))
        } else if lookahead.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            content.parse()
        } else if lookahead.peek(token::Bracket) {
            let content;
            syn::bracketed!(content in input);
            let mut elements = Vec::new();
            while !content.is_empty() {
                elements.push(content.parse()?);
                if !content.is_empty() {
                    content.parse::<Token![,]>()?;
                }
            }
            Ok(JsExpression::Array(elements))
        } else if lookahead.peek(token::Brace) {
            Ok(JsExpression::Object(input.parse()?))
        } else if lookahead.peek(function) {
            let func_decl: JsFunctionDecl = input.parse()?;
            Ok(JsExpression::Literal(JsLiteral::Function(func_decl)))
        } else {
            Err(lookahead.error())
        }
    }

    fn parse_postfix_expression(input: ParseStream) -> Result<JsExpression> {
        let mut expr = parse_primary_expression(input)?;
        loop {
            if input.peek(Token![.]) {
                input.parse::<Token![.]>()?;
                let member: Ident = input.parse()?;
                if input.peek(token::Paren) {
                    let content;
                    parenthesized!(content in input);
                    let args = content.parse_terminated(JsExpression::parse, Token![,])?;
                    expr = JsExpression::MethodCall {
                        object: Box::new(expr),
                        method: member,
                        args: args.into_iter().collect(),
                    };
                } else {
                    expr = JsExpression::MemberAccess(Box::new(expr), member);
                }
            } else if input.peek(token::Paren) {
                let content;
                parenthesized!(content in input);
                let args_parser = Punctuated::<JsExpression, Token![,]>::parse_terminated;
                let args = args_parser(&content)?;
                expr = JsExpression::Call(
                    match expr {
                        JsExpression::Identifier(id) => id,
                        _ => return Err(syn::Error::new_spanned(expr.to_token_stream(), "Expected function name")),
                    },
                    args.into_iter().collect(),
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_additive_expression(input: ParseStream) -> Result<JsExpression> {
        let mut expr = parse_postfix_expression(input)?;
        while input.peek(Token![+]) {
            let op = match input.parse::<Token![+]>() {
                Ok(_) => JsBinaryOp::Add,
                Err(e) => return Err(e),
            };
            let right = parse_postfix_expression(input)?;
            expr = JsExpression::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_comparison_expression(input: ParseStream) -> Result<JsExpression> {
        let mut expr = parse_additive_expression(input)?;
        while input.peek(Token![==])
            || input.peek(Token![!=])
            || input.peek(Token![<])
            || input.peek(Token![>])
            || input.peek(Token![<=])
            || input.peek(Token![>=])
        {
            let op = if input.peek(Token![==]) {
                input.parse::<Token![==]>()?;
                JsBinaryOp::Equal
            } else if input.peek(Token![!=]) {
                input.parse::<Token![!=]>()?;
                JsBinaryOp::NotEqual
            } else if input.peek(Token![<=]) {
                input.parse::<Token![<=]>()?;
                JsBinaryOp::LessThanOrEqual
            } else if input.peek(Token![>=]) {
                input.parse::<Token![>=]>()?;
                JsBinaryOp::GreaterThanOrEqual
            } else if input.peek(Token![<]) {
                input.parse::<Token![<]>()?;
                JsBinaryOp::LessThan
            } else if input.peek(Token![>]) {
                input.parse::<Token![>]>()?;
                JsBinaryOp::GreaterThan
            } else {
                // This should not be reached due to the `peek` checks
                return Err(syn::Error::new(input.span(), "Unsupported comparison operator"));
            };
            let right = parse_additive_expression(input)?;
            expr = JsExpression::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_assignment_expression(input: ParseStream) -> Result<JsExpression> {
        let left = parse_comparison_expression(input)?;
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            let right = parse_assignment_expression(input)?;
            if let JsExpression::Identifier(id) = left {
                return Ok(JsExpression::Assignment(id, Box::new(right)));
            } else {
                return Err(syn::Error::new_spanned(
                    left.to_token_stream(),
                    "Invalid assignment target",
                ));
            }
        }
        Ok(left)
    }

    impl Parse for JsExpression {
        fn parse(input: ParseStream) -> Result<Self> {
            parse_assignment_expression(input)
        }
    }

    impl TryFrom<Lit> for JsLiteral {
        type Error = syn::Error;
        fn try_from(lit: Lit) -> Result<Self> {
            match lit {
                Lit::Str(s) => Ok(JsLiteral::String(s.value())),
                Lit::Int(i) => Ok(JsLiteral::Number(i.base10_parse()?)),
                Lit::Float(f) => Ok(JsLiteral::Number(f.base10_parse()?)),
                Lit::Bool(b) => Ok(JsLiteral::Boolean(b.value)),
                _ => Err(syn::Error::new_spanned(lit, "Unsupported literal type")),
            }
        }
    }


    impl Parse for JsBlock {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            braced!(content in input);
            let mut statements = Vec::new();
            while !content.is_empty() {
                statements.push(content.parse()?);
            }
            Ok(JsBlock { statements })
        }
    }

    impl Parse for JsFunctionDecl {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<self::function>()?;
            let name = if input.peek(Ident) {
                Some(input.parse()?)
            } else {
                None
            };

            let content;
            parenthesized!(content in input);
            let params_parser = Punctuated::<Ident, Token![,]>::parse_terminated;
            let params = params_parser(&content)?;

            let body = input.parse()?;

            Ok(JsFunctionDecl {
                name,
                params: params.into_iter().collect(),
                body,
            })
        }
    }

    impl Parse for JsWhileLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<Token![while]>()?;
            let content;
            parenthesized!(content in input);
            let condition = content.parse()?;
            let body = input.parse()?;
            Ok(JsWhileLoop { condition, body })
        }
    }

    impl Parse for JsDoWhileLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<Token![do]>()?;
            let body = input.parse()?;
            input.parse::<Token![while]>()?;
            let content;
            parenthesized!(content in input);
            let condition = content.parse()?;
            input.parse::<Token![;]>()?;
            Ok(JsDoWhileLoop { body, condition })
        }
    }

    impl Parse for JsForLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<Token![for]>()?;
            let content;
            parenthesized!(content in input);

            // the initializer can be a let declaration or an expression.
            let init = if content.peek(Token![;]) {
                None
            } else {
                Some(Box::new(content.parse::<JsStatement>()?))
            };
            if !content.peek(Token![;]) {
                // if there was an init expression (not a declaration), it needs a semicolon.
                // JsStatement::parse already consumes it for declarations.
                if let Some(ref s) = init {
                    if let JsStatement::Expression(_) = **s {
                        content.parse::<Token![;]>()?;
                    }
                }
            } else {
                content.parse::<Token![;]>()?;
            }

            let condition = if content.peek(Token![;]) {
                None
            } else {
                Some(content.parse()?)
            };
            content.parse::<Token![;]>()?;

            let increment = if content.is_empty() {
                None
            } else {
                Some(Box::new(content.parse()?))
            };

            let body = input.parse()?;

            Ok(JsForLoop {
                init,
                condition,
                increment,
                body,
            })
        }
    }

    impl Parse for JsObject {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            braced!(content in input);
            Ok(JsObject {
                properties: content.parse_terminated(JsProperty::parse, Token![,])?,
            })
        }
    }

    impl Parse for JsProperty {
        fn parse(input: ParseStream) -> Result<Self> {
            let key = input.parse()?;
            input.parse::<Token![:]>()?;
            let value = input.parse()?;
            Ok(JsProperty { key, value })
        }
    }

    impl ToTokens for JsScript {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let mut functions = Vec::new();
            for stmt in &self.statements {
                if let JsStatement::FunctionDecl(func) = stmt {
                    functions.push(func.clone());
                }
            }

            let function_executors = functions.iter().map(|func| {
                let name = &func.name;
                let params = &func.params;
                let body = &func.body;

                quote! {
                fn #name(args: &[JsValue], scope: &std::cell::RefCell<std::collections::HashMap<String, JsValue>>) -> JsValue {
                    // create a new scope for function execution
                    let func_scope = std::cell::RefCell::new({
                        let mut new_scope = scope.borrow().clone();
                        let param_names = vec![#(stringify!(#params)),*];
                        for (i, param_name) in param_names.iter().enumerate() {
                            if let Some(arg) = args.get(i) {
                                new_scope.insert(param_name.to_string(), arg.clone());
                            }
                        }
                        new_scope
                    });

                    // execute the function body
                    let scope = &func_scope;
                    #body
                    JsValue::Undefined
                }
            }
            });

            let statements = &self.statements;
            tokens.extend(quote! {
            #(#function_executors)*
            #(#statements)*
        });
        }
    }


    impl ToTokens for JsStatement {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsStatement::LetDecl(ident, expr) => {
                    tokens.extend(quote! {
                    {
                        let value = #expr;
                        let mut scope_borrow = scope.borrow_mut();
                        scope_borrow.insert(String::from(stringify!(#ident)), value);
                    }
                });
                }
                JsStatement::FunctionDecl(func) => {
                    let name = &func.name;
                    let params = &func.params;
                    let body = &func.body;

                    // store function definition in the scope
                    tokens.extend(quote! {
                    {
                        let mut scope_borrow = scope.borrow_mut();
                        scope_borrow.insert(String::from(stringify!(#name)), JsValue::Function(JsFunction {
                            parameters: vec![#(String::from(stringify!(#params))),*],
                            body: stringify!(#body).to_string(),
                        }));
                    }
                });
                }
                JsStatement::IfStatement(cond, body) => {
                    tokens.extend(quote! {
                    if (#cond).to_bool() #body
                });
                }
                JsStatement::ForLoop(fl) => {
                    tokens.extend(quote! { #fl; });
                }
                JsStatement::WhileLoop(wl) => {
                    tokens.extend(quote! { #wl; });
                }
                JsStatement::DoWhileLoop(dwl) => {
                    tokens.extend(quote! { #dwl; });
                }
                JsStatement::Expression(expr) => {
                    tokens.extend(quote! { #expr; });
                }
            }
        }
    }


    impl ToTokens for JsExpression {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsExpression::Literal(lit) => {
                    tokens.extend(quote! { #lit });
                }
                JsExpression::Identifier(ident) => {
                    tokens.extend(quote! {
                    {
                        let scope_borrow = scope.borrow();
                        scope_borrow.get(stringify!(#ident)).unwrap().clone()
                    }
                });
                }
                JsExpression::BinaryOp(left, op, right) => {
                    let method_name = match op {
                        JsBinaryOp::Add => quote! { add },
                        JsBinaryOp::Equal => quote! { equals },
                        JsBinaryOp::NotEqual => quote! { not_equals },
                        JsBinaryOp::LessThan => quote! { less_than },
                        JsBinaryOp::GreaterThan => quote! { greater_than },
                        JsBinaryOp::LessThanOrEqual => quote! { less_than_or_equal },
                        JsBinaryOp::GreaterThanOrEqual => quote! { greater_than_or_equal },
                    };
                    tokens.extend(quote! {
                    (#left).#method_name(&#right)
                });
                }
                JsExpression::Assignment(ident, expr) => {
                    tokens.extend(quote! {
                    {
                        let value = #expr;
                        {
                            let mut scope_borrow = scope.borrow_mut();
                            scope_borrow.insert(String::from(stringify!(#ident)), value.clone());
                        }
                        value
                    }
                });
                }
                JsExpression::Call(ident, args) => {
                    tokens.extend(quote! {
                    {
                        let args_vec = vec![#(#args),*];
                        #ident(&args_vec, &scope)
                    }
                });
                }
                JsExpression::Array(elements) => {
                    tokens.extend(quote! {
                    JsValue::Array(vec![#(#elements),*])
                });
                }
                JsExpression::Object(obj) => {
                    tokens.extend(quote! { #obj });
                }
                JsExpression::MemberAccess(obj, member) => {
                    tokens.extend(quote! {
                    {
                        let obj_val = #obj;
                        if let JsValue::Object(ref map) = obj_val {
                            map.get(stringify!(#member)).unwrap_or(&JsValue::Undefined).clone()
                        } else {
                            JsValue::Undefined
                        }
                    }
                });
                }
                JsExpression::MethodCall { object, method, args } => {
                    // console.log
                    if let JsExpression::Identifier(obj_ident) = &**object {
                        if obj_ident == "console" && method == "log" {
                            tokens.extend(quote! {
                            {
                                let console_args = vec![#(#args),*];
                                console_log(console_args);
                                JsValue::Undefined
                            }
                        });
                            return;
                        }
                    }

                    let obj_ident = match &**object {
                        JsExpression::Identifier(id) => id,
                        _ => panic!("Only simple identifiers allowed on the left-hand side of a method call"),
                    };

                    let method_str = method.to_string();
                    if method_str == "push" {
                        tokens.extend(quote! {
                        {
                            let method_args = vec![#(#args),*];
                            let result = {
                                let mut scope_borrow = scope.borrow_mut();
                                let value = scope_borrow.get_mut(stringify!(#obj_ident)).expect("undefined identifier");
                                if let Some(arg) = method_args.into_iter().next() {
                                    value.push(arg)
                                } else {
                                    JsValue::Undefined
                                }
                            };
                            result
                        }
                    });
                    } else if method_str == "pop" {
                        tokens.extend(quote! {
                        {
                            let result = {
                                let mut scope_borrow = scope.borrow_mut();
                                let value = scope_borrow.get_mut(stringify!(#obj_ident)).expect("undefined identifier");
                                value.pop()
                            };
                            result
                        }
                    });
                    } else {
                        // function property call (like person.name())
                        tokens.extend(quote! {
                        {
                            let obj = {
                                let scope_borrow = scope.borrow();
                                scope_borrow.get(stringify!(#obj_ident)).expect("undefined identifier").clone()
                            };
                            if let JsValue::Object(ref map) = obj {
                                if let Some(JsValue::Function(_func)) = map.get(stringify!(#method)) {
                                    let method_args = vec![#(#args),*];
                                    console_log(method_args);
                                    JsValue::Undefined
                                } else {
                                    JsValue::Undefined
                                }
                            } else {
                                JsValue::Undefined
                            }
                        }
                    });
                    }
                }
            }
        }
    }


    impl ToTokens for JsFunctionDecl {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let params_as_strings = self
                .params
                .iter()
                .map(|ident| quote! { String::from(stringify!(#ident)) });

            // we store the raw source of the body.
            let body_src = stringify!(#self);

            tokens.extend(quote! {
            JsFunction {
                parameters: vec![ #(#params_as_strings),* ],
                body: #body_src.to_string(),
            }
        });
        }
    }

    impl ToTokens for JsLiteral {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsLiteral::Number(n)      => tokens.extend(quote! { JsValue::Number(#n) }),
                JsLiteral::String(s)      => tokens.extend(quote! { JsValue::String(#s.to_string()) }),
                JsLiteral::Boolean(b)     => tokens.extend(quote! { JsValue::Boolean(#b) }),
                JsLiteral::Null           => tokens.extend(quote! { JsValue::Null }),
                JsLiteral::Undefined      => tokens.extend(quote! { JsValue::Undefined }),
                JsLiteral::Function(func) => tokens.extend(quote! { JsValue::Function(#func) }),
            }
        }
    }

    impl ToTokens for JsBlock {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let stmts = &self.statements;
            tokens.append_all(quote! {
                {
                    #(#stmts)*
                }
            });
        }
    }

    impl ToTokens for JsWhileLoop {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let cond = &self.condition;
            let body = &self.body;
            tokens.append_all(quote! {
                while (#cond).to_bool() {
                    #body
                }
            });
        }
    }

    impl ToTokens for JsDoWhileLoop {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let cond = &self.condition;
            let body = &self.body;
            tokens.append_all(quote! {
                loop {
                    #body
                    if !(#cond).to_bool() {
                        break;
                    }
                }
            });
        }
    }

    impl ToTokens for JsForLoop {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let init = &self.init;
            let condition = self
                .condition
                .as_ref()
                .map_or(quote!(JsValue::Boolean(true)), |c| quote!(#c));
            let increment = &self.increment;
            let body = &self.body;

            tokens.append_all(quote! {
                {
                    #init
                    loop {
                        if !(#condition).to_bool() {
                            break;
                        }
                        #body
                        #increment;
                    }
                }
            });
        }
    }

    impl ToTokens for JsObject {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let properties = self.properties.iter().map(|prop| {
                let key = prop.key.to_string();
                let value = &prop.value;
                quote! {
                    map.insert(#key.to_string(), #value);
                }
            });

            tokens.append_all(quote! {
                {
                    let mut map = std::collections::HashMap::new();
                    #(#properties)*
                    JsValue::Object(map)
                }
            });
        }
    }
}

#[proc_macro]
pub fn js(input: TokenStream) -> TokenStream {
    let script = parse_macro_input!(input as parser::JsScript);

    let expanded = quote! {
        {
            use js_runtime::{JsValue, console_log};
            use std::collections::HashMap;
            use std::cell::RefCell;

            let scope = RefCell::new(HashMap::<String, JsValue>::new());

            #script
        }
    };

    TokenStream::from(expanded)
}
