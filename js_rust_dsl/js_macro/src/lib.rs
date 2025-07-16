extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod parser {
    use proc_macro2::TokenStream as TokenStream2;
    use quote::ToTokens;
    use syn::parse::{Parse, ParseStream};
    use syn::{braced, custom_keyword, parenthesized, Ident, Lit, Result, Token};

    custom_keyword!(function);

    pub struct JsScript {
        statements: Vec<JsStatement>,
    }

    #[derive(Clone)]
    pub enum JsStatement {
        LetDecl(Ident, JsExpression),
        ConsoleLog(JsExpression),
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
    }

    #[derive(Clone)]
    pub struct JsBlock {
        statements: Vec<JsStatement>,
    }

    #[derive(Clone)]
    pub struct JsFunctionDecl {
        name: Ident,
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
                if input.peek(Token![;]) {
                    let _ = input.parse::<Token![;]>()?;
                }
            }
            Ok(JsScript { statements })
        }
    }

    impl Parse for JsStatement {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();

            if lookahead.peek(Token![let]) {
                let _ = input.parse::<Token![let]>()?;
                let var_name: Ident = input.parse()?;
                let _ = input.parse::<Token![=]>()?;
                let value: JsExpression = input.parse()?;
                Ok(JsStatement::LetDecl(var_name, value))
            } else if lookahead.peek(Token![if]) {
                let _ = input.parse::<Token![if]>()?;
                let condition_content;
                parenthesized!(condition_content in input);
                let condition_expr: JsExpression = condition_content.parse()?;
                let block: JsBlock = input.parse()?;
                Ok(JsStatement::IfStatement(condition_expr, block))
            } else if lookahead.peek(function) {
                Ok(JsStatement::FunctionDecl(input.parse()?))
            } else if lookahead.peek(Token![while]) {
                Ok(JsStatement::WhileLoop(input.parse()?))
            } else if lookahead.peek(Token![do]) {
                Ok(JsStatement::DoWhileLoop(input.parse()?))
            } else if lookahead.peek(Token![for]) {
                Ok(JsStatement::ForLoop(input.parse()?))
            } else if lookahead.peek(Ident) {
                let fork = input.fork();
                let ident: Ident = fork.parse()?;
                if ident == "console" {
                    let _ = input.parse::<Ident>()?;
                    let _ = input.parse::<Token![.]>()?;
                    let _ = input.parse::<Ident>()?;
                    let content;
                    parenthesized!(content in input);
                    let expr: JsExpression = content.parse()?;
                    return Ok(JsStatement::ConsoleLog(expr));
                }
                Ok(JsStatement::Expression(input.parse()?))
            } else {
                Err(lookahead.error())
            }
        }
    }

    fn parse_primary_expression(input: ParseStream) -> Result<JsExpression> {
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

    fn parse_call_expression(input: ParseStream) -> Result<JsExpression> {
        let mut expr = parse_primary_expression(input)?;
        if input.peek(syn::token::Paren) {
            if let JsExpression::Identifier(ident) = expr.clone() {
                let args_content;
                parenthesized!(args_content in input);
                let mut args = Vec::new();
                while !args_content.is_empty() {
                    args.push(args_content.parse()?);
                    if args_content.peek(Token![,]) {
                        args_content.parse::<Token![,]>()?;
                    }
                }
                expr = JsExpression::Call(ident, args);
            }
        }
        Ok(expr)
    }

    fn parse_additive_expression(input: ParseStream) -> Result<JsExpression> {
        let mut lhs = parse_call_expression(input)?;
        while input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
            let op = JsBinaryOp::Add;
            let rhs = parse_call_expression(input)?;
            lhs = JsExpression::BinaryOp(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_comparison_expression(input: ParseStream) -> Result<JsExpression> {
        let mut lhs = parse_additive_expression(input)?;
        loop {
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
                break;
            };
            let rhs = parse_additive_expression(input)?;
            lhs = JsExpression::BinaryOp(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_assignment_expression(input: ParseStream) -> Result<JsExpression> {
        let fork = input.fork();
        let lhs = parse_comparison_expression(input)?;

        if input.peek(Token![=]) && !input.peek(Token![==]) {
            if let JsExpression::Identifier(ident) = lhs {
                input.parse::<Token![=]>()?;
                let rhs = parse_assignment_expression(input)?;
                return Ok(JsExpression::Assignment(ident, Box::new(rhs)));
            } else {
                return Err(fork.error("Invalid assignment target"));
            }
        }

        Ok(lhs)
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
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            braced!(content in input);
            let mut statements = Vec::new();
            while !content.is_empty() {
                statements.push(content.parse()?);
                if content.peek(Token![;]) {
                    let _ = content.parse::<Token![;]>()?;
                }
            }
            Ok(JsBlock { statements })
        }
    }

    impl Parse for JsFunctionDecl {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<function>()?;
            let name = input.parse()?;
            let params_content;
            parenthesized!(params_content in input);
            let mut params = Vec::new();
            while !params_content.is_empty() {
                params.push(params_content.parse()?);
                if params_content.peek(Token![,]) {
                    params_content.parse::<Token![,]>()?;
                }
            }
            let body = input.parse()?;
            Ok(JsFunctionDecl { name, params, body })
        }
    }

    impl Parse for JsWhileLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<Token![while]>()?;
            let condition_content;
            parenthesized!(condition_content in input);
            let condition = condition_content.parse()?;
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
            Ok(JsDoWhileLoop { body, condition })
        }
    }

    impl Parse for JsForLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            input.parse::<Token![for]>()?;
            let content;
            parenthesized!(content in input);

            let init = if !content.peek(Token![;]) {
                Some(Box::new(content.parse()?))
            } else {
                None
            };
            content.parse::<Token![;]>()?;

            let condition = if !content.peek(Token![;]) {
                Some(content.parse()?)
            } else {
                None
            };
            content.parse::<Token![;]>()?;

            let increment = if !content.is_empty() {
                Some(Box::new(content.parse()?))
            } else {
                None
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

    impl ToTokens for JsScript {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            for statement in &self.statements {
                statement.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for JsStatement {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsStatement::LetDecl(var_name, value_expr) => {
                    tokens.extend(quote::quote! {
                        let mut #var_name: js_runtime::JsValue = #value_expr;
                    });
                }
                JsStatement::ConsoleLog(expr) => {
                    tokens.extend(quote::quote! {
                        js_runtime::console_log(&#expr);
                    });
                }
                JsStatement::IfStatement(condition_expr, block) => {
                    tokens.extend(quote::quote! {
                        if #condition_expr.to_bool() {
                            #block
                        }
                    });
                }
                JsStatement::FunctionDecl(func) => func.to_tokens(tokens),
                JsStatement::WhileLoop(loop_stmt) => loop_stmt.to_tokens(tokens),
                JsStatement::DoWhileLoop(loop_stmt) => loop_stmt.to_tokens(tokens),
                JsStatement::ForLoop(loop_stmt) => loop_stmt.to_tokens(tokens),
                JsStatement::Expression(expr) => {
                    tokens.extend(quote::quote! {
                        let _ = #expr;
                    });
                }
            }
        }
    }

    impl ToTokens for JsExpression {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsExpression::Literal(lit) => lit.to_tokens(tokens),
                JsExpression::Identifier(ident) => {
                    tokens.extend(quote::quote! { #ident.clone() });
                }
                JsExpression::BinaryOp(lhs, op, rhs) => {
                    let op_token = match op {
                        JsBinaryOp::Add => quote::quote! { .add },
                        JsBinaryOp::Equal => quote::quote! { .equals },
                        JsBinaryOp::NotEqual => quote::quote! { .not_equals },
                        JsBinaryOp::LessThan => quote::quote! { .less_than },
                        JsBinaryOp::GreaterThan => quote::quote! { .greater_than },
                        JsBinaryOp::LessThanOrEqual => quote::quote! { .less_than_or_equal },
                        JsBinaryOp::GreaterThanOrEqual => quote::quote! { .greater_than_or_equal },
                    };
                    tokens.extend(quote::quote! {
                        (#lhs)#op_token(&#rhs)
                    });
                }
                JsExpression::Assignment(ident, value) => {
                    tokens.extend(quote::quote! {{
                        #ident = #value;
                        #ident.clone()
                    }});
                }
                JsExpression::Call(func_name, args) => {
                    tokens.extend(quote::quote! {
                        #func_name(#(#args),*)
                    });
                }
            }
        }
    }

    impl ToTokens for JsLiteral {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                JsLiteral::Number(n) => {
                    tokens.extend(quote::quote! { js_runtime::JsValue::Number(#n) })
                }
                JsLiteral::String(s) => {
                    tokens.extend(quote::quote! { js_runtime::JsValue::String(#s.to_string()) })
                }
                JsLiteral::Boolean(b) => {
                    tokens.extend(quote::quote! { js_runtime::JsValue::Boolean(#b) })
                }
                JsLiteral::Null => tokens.extend(quote::quote! { js_runtime::JsValue::Null }),
                JsLiteral::Undefined => {
                    tokens.extend(quote::quote! { js_runtime::JsValue::Undefined })
                }
            }
        }
    }

    impl ToTokens for JsBlock {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let statements = &self.statements;
            tokens.extend(quote::quote! {
                {
                    #(#statements)*
                }
            });
        }
    }

    impl ToTokens for JsFunctionDecl {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let name = &self.name;
            let params = &self.params;
            let body = &self.body;
            tokens.extend(quote::quote! {
                #[allow(unused_mut, unused_variables)]
                fn #name(#(mut #params: js_runtime::JsValue),*) -> js_runtime::JsValue {
                    #body
                    js_runtime::JsValue::Undefined
                }
            });
        }
    }

    impl ToTokens for JsWhileLoop {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let condition = &self.condition;
            let body = &self.body;
            tokens.extend(quote::quote! {
                while #condition.to_bool() {
                    #body
                }
            });
        }
    }

    impl ToTokens for JsDoWhileLoop {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let body = &self.body;
            let condition = &self.condition;
            tokens.extend(quote::quote! {
                loop {
                    #body
                    if !(#condition.to_bool()) {
                        break;
                    }
                }
            });
        }
    }

    impl ToTokens for JsForLoop {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            let init = self.init.as_ref().map(|s| quote::quote!(#s));
            let condition = self
                .condition
                .as_ref()
                .map(|c| quote::quote!(#c.to_bool()))
                .unwrap_or(quote::quote!(true));
            let increment = self.increment.as_ref().map(|i| quote::quote!(let _ = #i;));
            let body = &self.body;
            tokens.extend(quote::quote! {
                {
                    #init
                    while #condition {
                        #body
                        #increment
                    }
                }
            });
        }
    }
}

#[proc_macro]
pub fn js_script(input: TokenStream) -> TokenStream {
    let script = parse_macro_input!(input as parser::JsScript);
    let expanded = quote! {
        #script
    };
    expanded.into()
}