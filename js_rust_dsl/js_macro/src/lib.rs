
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

    #[derive(Clone, Debug)]
    pub enum JsStatement {
        LetDecl(Ident, JsExpression),
        IfStatement(JsExpression, JsBlock),
        FunctionDecl(JsFunctionDecl),
        ForLoop(JsForLoop),
        WhileLoop(JsWhileLoop),
        DoWhileLoop(JsDoWhileLoop),
        Expression(JsExpression),
    }

    #[derive(Clone, Debug)]
    pub enum JsExpression {
        Literal(JsLiteral),
        Identifier(Ident),
        This,
        BinaryOp(Box<JsExpression>, JsBinaryOp, Box<JsExpression>),
        Assignment(Ident, Box<JsExpression>),
        MemberAssignment(Box<JsExpression>, Ident, Box<JsExpression>),
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

    impl std::fmt::Debug for JsObject {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("JsObject")
                .field("properties", &self.properties.iter().collect::<Vec<_>>())
                .finish()
        }
    }

    #[derive(Clone)]
    pub struct JsProperty {
        key: Ident,
        value: JsExpression,
    }

    impl std::fmt::Debug for JsProperty {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("JsProperty")
                .field("key", &self.key)
                .field("value", &self.value)
                .finish()
        }
    }

    #[derive(Clone, Debug)]
    pub enum JsBinaryOp {
        Add,
        Equal,
        NotEqual,
        LessThan,
        GreaterThan,
        LessThanOrEqual,
        GreaterThanOrEqual,
    }

    #[derive(Clone, Debug)]
    pub enum JsLiteral {
        Number(f64),
        String(String),
        Boolean(bool),
        Null,
        Undefined,
        Function(JsFunctionDecl),
    }

    #[derive(Clone, Debug)]
    pub struct JsBlock {
        statements: Vec<JsStatement>,
    }

    #[derive(Clone, Debug)]
    pub struct JsFunctionDecl {
        name: Option<Ident>,
        params: Vec<Ident>,
        body: JsBlock,
    }

    #[derive(Clone, Debug)]
    pub struct JsWhileLoop {
        condition: JsExpression,
        body: JsBlock,
    }

    #[derive(Clone, Debug)]
    pub struct JsDoWhileLoop {
        body: JsBlock,
        condition: JsExpression,
    }

    #[derive(Clone, Debug)]
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
            let ident: Ident = input.parse()?;
            if ident == "this" {
                Ok(JsExpression::This)
            } else {
                Ok(JsExpression::Identifier(ident))
            }
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

            return match left {
                JsExpression::Identifier(id) => {
                    Ok(JsExpression::Assignment(id, Box::new(right)))
                }
                JsExpression::MemberAccess(obj, prop) => {
                    Ok(JsExpression::MemberAssignment(obj, prop, Box::new(right)))
                }
                _ => {
                    Err(syn::Error::new_spanned(
                        left.to_token_stream(),
                        "Invalid assignment target",
                    ))
                }
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

            let init = if content.peek(Token![;]) {
                None
            } else {
                Some(Box::new(content.parse::<JsStatement>()?))
            };
            if !content.peek(Token![;]) {
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
            let statements = &self.statements;
            tokens.extend(quote! {
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
                    if let Some(name) = &func.name {
                        let params = &func.params;
                        let body_str = format!("{{ /* function body */ }}");
                        tokens.extend(quote! {
                            {
                                let mut scope_borrow = scope.borrow_mut();
                                scope_borrow.insert(String::from(stringify!(#name)), JsValue::Function(JsFunction::new(
                                    vec![#(String::from(stringify!(#params))),*],
                                    Vec::new(),
                                    #body_str.to_string()
                                )));
                            }
                        });
                    }
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
                JsExpression::This => {
                    tokens.extend(quote! {
                    {
                        if let Some(this_ref) = this_context {
                            this_ref.borrow().clone()
                        } else {
                            JsValue::Undefined
                        }
                    }
                });
                }
                JsExpression::Identifier(ident) => {
                    tokens.extend(quote! {
                    {
                        let scope_borrow = scope.borrow();
                        scope_borrow.get(stringify!(#ident)).unwrap_or(&JsValue::Undefined).clone()
                    }
                });
                }
                JsExpression::MemberAssignment(_obj, _prop, value) => {
                    tokens.extend(quote! {
                    {
                        let new_value = #value;
                        new_value
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
                        let scope_borrow = scope.borrow();
                        if let Some(JsValue::Function(func_ref)) = scope_borrow.get(stringify!(#ident)) {
                            let func_clone = func_ref.clone();
                            drop(scope_borrow);
                            func_clone.execute(&args_vec, &scope, None)
                        } else {
                            JsValue::Undefined
                        }
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
                        obj_val.get_property(stringify!(#member))
                    }
                });
                }
                JsExpression::MethodCall { object, method, args } => {
                    if let JsExpression::Identifier(obj_ident) = object.as_ref() {
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

                    //array methods
                    let method_str = method.to_string();
                    if method_str == "push" || method_str == "pop" {
                        if let JsExpression::Identifier(obj_ident) = object.as_ref() {
                            if method_str == "push" {
                                tokens.extend(quote! {
                                    {
                                    let method_args = vec![#(#args),*];
                                    let result = {
                                        let mut scope_borrow = scope.borrow_mut();
                                        if let Some(value) = scope_borrow.get_mut(stringify!(#obj_ident)) {
                                            if let Some(arg) = method_args.into_iter().next() {
                                                value.push(arg)
                                            } else {
                                                JsValue::Undefined
                                            }
                                        } else {
                                            JsValue::Undefined
                                        }
                                    };
                                        result
                                    }
                                });
                            } else {
                                tokens.extend(quote! {
                                    {
                                        let result = {
                                            let mut scope_borrow = scope.borrow_mut();
                                            if let Some(value) = scope_borrow.get_mut(stringify!(#obj_ident)) {
                                                value.pop()
                                            } else {
                                                JsValue::Undefined
                                            }
                                        };
                                        result
                                    }
                                });
                            }

                            return;
                        }
                    }

                    // works for all methods
                    if let JsExpression::Identifier(obj_ident) = object.as_ref() {
                        tokens.extend(quote! {
                            {
                                let method_args = vec![#(#args),*];
                                let method_name = stringify!(#method);

                                let (method_result, updated_obj) = {
                                    let scope_borrow = scope.borrow();
                                    let object_val = scope_borrow.get(stringify!(#obj_ident)).unwrap_or(&JsValue::Undefined).clone();
                                    drop(scope_borrow);
                                    object_val.execute_method(method_name, method_args, &scope)
                                };


                                {
                                    let mut scope_borrow = scope.borrow_mut();
                                    scope_borrow.insert(stringify!(#obj_ident).to_string(), updated_obj);
                                }

                                method_result
                            }
                        });
                    } else {
                        // on expressions (not identifiers)
                        tokens.extend(quote! {
                            {
                                let method_args = vec![#(#args),*];
                                let object_val = #object;
                                let (method_result, _updated_obj) = object_val.execute_method(stringify!(#method), method_args, &scope);
                                method_result
                            }
                        });
                    }
                }
            }
        }
    }

    impl ToTokens for JsLiteral {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsLiteral::Number(n) => tokens.extend(quote! { JsValue::Number(#n) }),
                JsLiteral::String(s) => tokens.extend(quote! { JsValue::String(#s.to_string()) }),
                JsLiteral::Boolean(b) => tokens.extend(quote! { JsValue::Boolean(#b) }),
                JsLiteral::Null => tokens.extend(quote! { JsValue::Null }),
                JsLiteral::Undefined => tokens.extend(quote! { JsValue::Undefined }),
                JsLiteral::Function(func) => {
                    let params = &func.params;
                    let body_statements = &func.body.statements;

                    let runtime_statements = body_statements.iter().map(|stmt| {
                        convert_statement_to_runtime(stmt)
                    });

                    tokens.extend(quote! {
                    JsValue::Function(JsFunction::new(
                        vec![#(String::from(stringify!(#params))),*],
                        vec![#(#runtime_statements),*],
                        format!("function({}) {{ ... }}", vec![#(stringify!(#params)),*].join(", "))
                    ))
                });
                }
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

    impl ToTokens for JsFunctionDecl {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let params = &self.params;
            let body = &self.body;
            if let Some(name) = &self.name {
                tokens.extend(quote! {
                function #name(#(#params),*) #body
            });
            } else {
                tokens.extend(quote! {
                function(#(#params),*) #body
            });
            }
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

            tokens.extend(quote! {
                {
                    let mut map = std::collections::HashMap::new();
                    #(#properties)*
                    JsValue::Object(map)
                }
            });
        }
    }
}

fn convert_statement_to_runtime(stmt: &parser::JsStatement) -> proc_macro2::TokenStream {
    match stmt {
        parser::JsStatement::Expression(expr) => {
            let runtime_expr = convert_expression_to_runtime(expr);
            quote! { js_runtime::JsStatement::Expression(#runtime_expr) }
        }
        parser::JsStatement::LetDecl(ident, expr) => {
            let ident_str = ident.to_string();
            let runtime_expr = convert_expression_to_runtime(expr);
            quote! { js_runtime::JsStatement::Assignment(#ident_str.to_string(), #runtime_expr) }
        }
        parser::JsStatement::FunctionDecl(_) => {
            // function declarations at statement level arent handled in object methods
            quote! { js_runtime::JsStatement::Expression(js_runtime::JsExpression::Literal(js_runtime::JsValue::Undefined)) }
        }
        parser::JsStatement::IfStatement(cond, _body) => {
            let runtime_cond = convert_expression_to_runtime(cond);
            // converts to an expression for now
            quote! { js_runtime::JsStatement::Expression(#runtime_cond) }
        }
        _ => quote! { js_runtime::JsStatement::Expression(js_runtime::JsExpression::Literal(js_runtime::JsValue::Undefined)) }
    }
}

fn convert_expression_to_runtime(expr: &parser::JsExpression) -> proc_macro2::TokenStream {
    match expr {
        parser::JsExpression::Literal(lit) => {
            match lit {
                parser::JsLiteral::String(s) => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::String(#s.to_string())) },
                parser::JsLiteral::Number(n) => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::Number(#n)) },
                parser::JsLiteral::Boolean(b) => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::Boolean(#b)) },
                parser::JsLiteral::Null => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::Null) },
                parser::JsLiteral::Undefined => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::Undefined) },
                _ => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::Undefined) }
            }
        }
        parser::JsExpression::Identifier(ident) => {
            let ident_str = ident.to_string();
            quote! { js_runtime::JsExpression::Identifier(#ident_str.to_string()) }
        }
        parser::JsExpression::This => {
            quote! { js_runtime::JsExpression::This }
        }
        parser::JsExpression::BinaryOp(left, op, right) => {
            let left_runtime = convert_expression_to_runtime(left);
            let right_runtime = convert_expression_to_runtime(right);
            let op_str = match op {
                parser::JsBinaryOp::Add => "+",
                parser::JsBinaryOp::Equal => "==",
                parser::JsBinaryOp::NotEqual => "!=",
                parser::JsBinaryOp::LessThan => "<",
                parser::JsBinaryOp::GreaterThan => ">",
                parser::JsBinaryOp::LessThanOrEqual => "<=",
                parser::JsBinaryOp::GreaterThanOrEqual => ">=",
            };
            quote! {
                js_runtime::JsExpression::BinaryOp(
                    Box::new(#left_runtime),
                    #op_str.to_string(),
                    Box::new(#right_runtime)
                )
            }
        }
        parser::JsExpression::Assignment(ident, expr) => {
            let ident_str = ident.to_string();
            let runtime_expr = convert_expression_to_runtime(expr);
            quote! {
                js_runtime::JsExpression::Call(
                    "__assign__".to_string(),
                    vec![
                        js_runtime::JsExpression::Literal(js_runtime::JsValue::String(#ident_str.to_string())),
                        #runtime_expr
                    ]
                )
            }
        }
        parser::JsExpression::MemberAssignment(obj, prop, value) => {
            let obj_runtime = convert_expression_to_runtime(obj);
            let prop_str = prop.to_string();
            let value_runtime = convert_expression_to_runtime(value);

            // convert this.prop = value to a special method call
            quote! {
                js_runtime::JsExpression::MethodCall {
                    object: Box::new(#obj_runtime),
                    method: "__set_property__".to_string(),
                    args: vec![
                        js_runtime::JsExpression::Literal(js_runtime::JsValue::String(#prop_str.to_string())),
                        #value_runtime
                    ]
                }
            }
        }
        parser::JsExpression::Call(ident, args) => {
            let ident_str = ident.to_string();
            let runtime_args: Vec<_> = args.iter().map(convert_expression_to_runtime).collect();
            quote! {
                js_runtime::JsExpression::Call(
                    #ident_str.to_string(),
                    vec![#(#runtime_args),*]
                )
            }
        }
        parser::JsExpression::MethodCall { object, method, args } => {
            let obj_runtime = convert_expression_to_runtime(object);
            let method_str = method.to_string();
            let runtime_args: Vec<_> = args.iter().map(convert_expression_to_runtime).collect();
            quote! {
                js_runtime::JsExpression::MethodCall {
                    object: Box::new(#obj_runtime),
                    method: #method_str.to_string(),
                    args: vec![#(#runtime_args),*]
                }
            }
        }
        parser::JsExpression::MemberAccess(obj, member) => {
            let obj_runtime = convert_expression_to_runtime(obj);
            let member_str = member.to_string();
            quote! {
                js_runtime::JsExpression::MemberAccess(
                    Box::new(#obj_runtime),
                    #member_str.to_string()
                )
            }
        }
        _ => quote! { js_runtime::JsExpression::Literal(js_runtime::JsValue::Undefined) }
    }
}


#[proc_macro]
pub fn js(input: TokenStream) -> TokenStream {
    let script = parse_macro_input!(input as parser::JsScript);

    let expanded = quote! {
        {
            use js_runtime::{JsValue, JsFunction, console_log};
            use std::collections::HashMap;
            use std::cell::RefCell;

            let scope = RefCell::new(HashMap::<String, JsValue>::new());
            let this_context: Option<&RefCell<JsValue>> = None;

            #script
        }
    };

    TokenStream::from(expanded)
}