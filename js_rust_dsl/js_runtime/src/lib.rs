use std::collections::HashMap;
use std::fmt;
use std::cell::RefCell;

#[derive(Clone, Debug)]
pub enum JsStatement {
    Expression(JsExpression),
    Assignment(String, JsExpression),
    MemberAssignment(JsExpression, String, JsExpression),
    Return(Option<JsExpression>),
}

#[derive(Clone, Debug)]
pub enum JsExpression {
    Literal(JsValue),
    Identifier(String),
    This,
    BinaryOp(Box<JsExpression>, String, Box<JsExpression>),
    Call(String, Vec<JsExpression>),
    MemberAccess(Box<JsExpression>, String),
    MethodCall {
        object: Box<JsExpression>,
        method: String,
        args: Vec<JsExpression>,
    },
}

#[derive(Clone, Debug)]
pub struct JsFunction {
    pub parameters: Vec<String>,
    pub body: Vec<JsStatement>, // Executable AST 
    pub body_string: String, // for display
}

pub struct JsExecutionContext<'a> {
    pub scope: &'a RefCell<HashMap<String, JsValue>>,
    pub this_context: Option<&'a RefCell<JsValue>>,
}

impl JsFunction {
    pub fn new(parameters: Vec<String>, body: Vec<JsStatement>, body_string: String) -> Self {
        JsFunction {
            parameters,
            body,
            body_string,
        }
    }

    pub fn execute(
        &self,
        args: &[JsValue],
        global_scope: &RefCell<HashMap<String, JsValue>>,
        this_context: Option<&RefCell<JsValue>>
    ) -> JsValue {
        // Params bound
        let func_scope = RefCell::new({
            let mut new_scope = global_scope.borrow().clone();
            for (i, param_name) in self.parameters.iter().enumerate() {
                if let Some(arg) = args.get(i) {
                    new_scope.insert(param_name.clone(), arg.clone());
                }
            }
            new_scope
        });

        let context = JsExecutionContext {
            scope: &func_scope,
            this_context,
        };
        
        let mut result = JsValue::Undefined;
        for statement in &self.body {
            result = statement.execute(&context); 
        }

        result
    }
}

impl JsStatement {
    pub fn execute(&self, context: &JsExecutionContext) -> JsValue {
        match self {
            JsStatement::Expression(expr) => expr.evaluate(context),
            JsStatement::Assignment(name, expr) => {
                let value = expr.evaluate(context);
                context.scope.borrow_mut().insert(name.clone(), value.clone());
                value
            }
            JsStatement::MemberAssignment(obj_expr, prop, value_expr) => {
                let value = value_expr.evaluate(context);
                match obj_expr {
                    JsExpression::This => {
                        if let Some(this_ref) = context.this_context {
                            this_ref.borrow_mut().set_property(prop, value.clone());
                        }
                    }
                    JsExpression::Identifier(var_name) => {
                        // obj.prop = value where obj is a variable
                        if let Some(JsValue::Object(_)) = context.scope.borrow().get(var_name) {
                            let mut scope_borrow = context.scope.borrow_mut();
                            if let Some(obj_val) = scope_borrow.get_mut(var_name) {
                                obj_val.set_property(prop, value.clone());
                            }
                        }
                    }
                    _ => {
                        // other object references
                    }
                }
                value
            }
            JsStatement::Return(expr_opt) => {
                expr_opt.as_ref().map_or(JsValue::Undefined, |expr| expr.evaluate(context))
            }
        }
    }
}

impl JsExpression {
    pub fn evaluate(&self, context: &JsExecutionContext) -> JsValue {
        match self {
            JsExpression::Literal(value) => value.clone(),
            JsExpression::Identifier(name) => {
                context.scope.borrow().get(name).cloned().unwrap_or(JsValue::Undefined)
            }
            JsExpression::This => {
                if let Some(this_ref) = context.this_context {
                    this_ref.borrow().clone()
                } else {
                    JsValue::Undefined
                }
            }
            JsExpression::BinaryOp(left, op, right) => {
                let left_val = left.evaluate(context);
                let right_val = right.evaluate(context);
                match op.as_str() {
                    "+" => left_val.add(&right_val),
                    "==" => left_val.equals(&right_val),
                    "!=" => left_val.not_equals(&right_val),
                    "<" => left_val.less_than(&right_val),
                    ">" => left_val.greater_than(&right_val),
                    "<=" => left_val.less_than_or_equal(&right_val),
                    ">=" => left_val.greater_than_or_equal(&right_val),
                    _ => JsValue::Undefined,
                }
            }
            JsExpression::Call(func_name, args) => {
                let arg_values: Vec<JsValue> = args.iter().map(|arg| arg.evaluate(context)).collect();
                
                if func_name == "__assign__" {
                    if let (Some(JsValue::String(var_name)), Some(value)) = (arg_values.get(0), arg_values.get(1)) {
                        context.scope.borrow_mut().insert(var_name.clone(), value.clone());
                        return value.clone();
                    }
                    return JsValue::Undefined;
                }

                if func_name == "console" {
                    console_log(arg_values);
                    JsValue::Undefined
                } else {
                    if let Some(JsValue::Function(func)) = context.scope.borrow().get(func_name) {
                        let func_clone = func.clone();
                        func_clone.execute(&arg_values, context.scope, context.this_context)
                    } else {
                        JsValue::Undefined
                    }
                }
            }
            JsExpression::MemberAccess(obj_expr, member) => {
                let obj = obj_expr.evaluate(context);
                obj.get_property(member)
            }
            JsExpression::MethodCall { object, method, args } => {
                // special property assignment
                if method == "__set_property__" {
                    let obj = object.evaluate(context);
                    let arg_values: Vec<JsValue> = args.iter().map(|arg| arg.evaluate(context)).collect();

                    if let (Some(JsValue::String(prop_name)), Some(value)) = (arg_values.get(0), arg_values.get(1)) {
                        // If 'this', update the this object
                        if let JsExpression::This = object.as_ref() {
                            if let Some(this_ref) = context.this_context {
                                this_ref.borrow_mut().set_property(prop_name, value.clone());
                                return value.clone();
                            }
                        }
                        // Handle oother objects
                    }
                    return JsValue::Undefined;
                }
                
                if let JsExpression::Identifier(obj_name) = object.as_ref() {
                    if obj_name == "console" && method == "log" {
                        let arg_values: Vec<JsValue> = args.iter().map(|arg| arg.evaluate(context)).collect();
                        console_log(arg_values);
                        return JsValue::Undefined;
                    }
                }

                let obj = object.evaluate(context);
                let arg_values: Vec<JsValue> = args.iter().map(|arg| arg.evaluate(context)).collect();

                if let JsValue::Object(_) = obj {
                    let method_property = obj.get_property(method);
                    if let JsValue::Function(func) = method_property {
                        let this_context = RefCell::new(obj.clone());
                        let result = func.execute(&arg_values, context.scope, Some(&this_context));
                        
                        result
                    } else {
                        JsValue::Undefined
                    }
                } else {
                    JsValue::Undefined
                }
            }
        }
    }
}


#[derive(Clone, Debug)]
pub enum JsValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
    Object(HashMap<String, JsValue>),
    Array(Vec<JsValue>),
    Function(JsFunction),
}

impl JsValue {
    /// Converts a JsValue to a primitive string representation.
    /// This is used for operations like addition where coercion is needed.
    pub fn to_primitive_string(&self) -> String {
        match self {
            JsValue::String(s) => s.clone(),
            JsValue::Number(n) => n.to_string(),
            JsValue::Boolean(b) => b.to_string(),
            JsValue::Null => "null".to_string(),
            JsValue::Undefined => "undefined".to_string(),
            JsValue::Array(arr) => arr
                .iter()
                .map(|v| v.to_primitive_string())
                .collect::<Vec<_>>()
                .join(","),
            JsValue::Object(_) => "[object Object]".to_string(),
            JsValue::Function(func) => {
                format!("function({}) {{ {:?} }}", func.parameters.join(", "), func.body)
            }
        }
    }

    /// Converts a JsValue to the string format expected by `console.log`.
    /// Strings are not quoted, but other values are formatted as with Display.
    pub fn to_console_string(&self) -> String {
        match self {
            JsValue::String(s) => s.clone(),
            JsValue::Function(_) => self.to_primitive_string(),
            _ => self.to_string(),
        }
    }

    /// Implements JavaScript's addition operator.
    /// Handles numeric addition and string concatenation.
    pub fn add(&self, other: &JsValue) -> JsValue {
        if matches!(self, JsValue::String(_)) || matches!(other, JsValue::String(_)) {
            let s1 = self.to_primitive_string();
            let s2 = other.to_primitive_string();
            return JsValue::String(s1 + &s2);
        }

        JsValue::Number(self.to_number() + other.to_number())
    }

    /// Pushes a value to the end of a `JsValue::Array`.
    /// Returns the new length of the array.
    /// If the value is not an array, returns `Undefined`.
    pub fn push(&mut self, value: JsValue) -> JsValue {
        if let JsValue::Array(arr) = self {
            arr.push(value);
            JsValue::Number(arr.len() as f64)
        } else {
            JsValue::Undefined
        }
    }

    /// Removes and returns the last element of a `JsValue::Array`.
    /// If the array is empty or the value is not an array, returns `Undefined`.
    pub fn pop(&mut self) -> JsValue {
        if let JsValue::Array(arr) = self {
            arr.pop().unwrap_or(JsValue::Undefined)
        } else {
            JsValue::Undefined
        }
    }

    /// Implements JavaScript's abstract equality comparison (`==`).
    pub fn equals(&self, other: &JsValue) -> JsValue {
        let result = match (self, other) {
            // Strict equality cases
            (JsValue::Number(a), JsValue::Number(b)) => a == b,
            (JsValue::String(a), JsValue::String(b)) => a == b,
            (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
            (JsValue::Undefined, JsValue::Undefined) | (JsValue::Null, JsValue::Null) => true,
            // Coercion cases
            (JsValue::Null, JsValue::Undefined) | (JsValue::Undefined, JsValue::Null) => true,
            (JsValue::Number(_), JsValue::String(_)) | (JsValue::String(_), JsValue::Number(_)) => {
                self.to_number() == other.to_number()
            }
            (JsValue::Boolean(_), _) => self.to_number() == other.to_number(),
            (_, JsValue::Boolean(_)) => self.to_number() == other.to_number(),
            _ => false, // Default for objects, arrays, functions
        };
        JsValue::Boolean(result)
    }

    /// Implements JavaScript's inequality comparison (`!=`).
    pub fn not_equals(&self, other: &JsValue) -> JsValue {
        if let JsValue::Boolean(b) = self.equals(other) {
            JsValue::Boolean(!b)
        } else {
            // Should be unreachable
            JsValue::Boolean(true)
        }
    }

    /// Helper for relational comparisons, handles JS string vs number logic.
    fn compare_relational(&self, other: &JsValue) -> Option<std::cmp::Ordering> {
        if let (JsValue::String(s1), JsValue::String(s2)) = (self, other) {
            s1.partial_cmp(s2)
        } else {
            self.to_number().partial_cmp(&other.to_number())
        }
    }

    pub fn less_than(&self, other: &JsValue) -> JsValue {
        JsValue::Boolean(matches!(
            self.compare_relational(other),
            Some(std::cmp::Ordering::Less)
        ))
    }

    pub fn greater_than(&self, other: &JsValue) -> JsValue {
        JsValue::Boolean(matches!(
            self.compare_relational(other),
            Some(std::cmp::Ordering::Greater)
        ))
    }

    pub fn less_than_or_equal(&self, other: &JsValue) -> JsValue {
        JsValue::Boolean(!matches!(
            self.compare_relational(other),
            Some(std::cmp::Ordering::Greater)
        ))
    }

    pub fn greater_than_or_equal(&self, other: &JsValue) -> JsValue {
        JsValue::Boolean(!matches!(
            self.compare_relational(other),
            Some(std::cmp::Ordering::Less)
        ))
    }

    pub fn to_bool(&self) -> bool {
        match self {
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => *n != 0.0 && !n.is_nan(),
            JsValue::String(s) => !s.is_empty(),
            JsValue::Null | JsValue::Undefined => false,
            JsValue::Object(_) | JsValue::Array(_) | JsValue::Function(_) => true,
        }
    }

    pub fn to_number(&self) -> f64 {
        match self {
            JsValue::Number(n) => *n,
            JsValue::String(s) => s.parse::<f64>().unwrap_or(f64::NAN),
            JsValue::Boolean(true) => 1.0,
            JsValue::Boolean(false) => 0.0,
            JsValue::Null => 0.0,
            JsValue::Undefined => f64::NAN,
            _ => f64::NAN,
        }
    }

    /// Sets a property on an object, returns the value that was set
    pub fn set_property(&mut self, key: &str, value: JsValue) -> JsValue {
        if let JsValue::Object(map) = self {
            map.insert(key.to_string(), value.clone());
            value
        } else {
            JsValue::Undefined
        }
    }

    /// Gets a property from an object
    pub fn get_property(&self, key: &str) -> JsValue {
        if let JsValue::Object(map) = self {
            map.get(key).unwrap_or(&JsValue::Undefined).clone()
        } else {
            JsValue::Undefined
        }
    }

    pub fn as_function(&self) -> Option<&JsFunction> {
        match self {
            JsValue::Function(func) => Some(func),
            _ => None,
        }
    }


    /// Execute a method on this object with proper this binding
    pub fn execute_method(
        &self,
        method_name: &str,
        args: Vec<JsValue>,
        global_scope: &RefCell<HashMap<String, JsValue>>
    ) -> (JsValue, JsValue) {
        if let JsValue::Object(_) = self {
            let method_property = self.get_property(method_name);
            if let JsValue::Function(func) = method_property {
                let this_context = RefCell::new(self.clone());
                let result = func.execute(&args, global_scope, Some(&this_context));
                
                (result, this_context.into_inner())
            } else {
                (JsValue::Undefined, self.clone())
            }
        } else {
            (JsValue::Undefined, self.clone())
        }
    }

}

impl From<f64> for JsValue {
    fn from(num: f64) -> Self {
        JsValue::Number(num)
    }
}

impl From<String> for JsValue {
    fn from(s: String) -> Self {
        JsValue::String(s)
    }
}

impl From<&str> for JsValue {
    fn from(s: &str) -> Self {
        JsValue::String(s.to_string())
    }
}

impl From<bool> for JsValue {
    fn from(b: bool) -> Self {
        JsValue::Boolean(b)
    }
}

impl fmt::Display for JsValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsValue::Number(n) => write!(f, "{}", n),
            JsValue::String(s) => write!(f, "\"{}\"", s),
            JsValue::Boolean(b) => write!(f, "{}", b),
            JsValue::Null => write!(f, "null"),
            JsValue::Undefined => write!(f, "undefined"),
            JsValue::Object(_) => write!(f, "[object Object]"),
            JsValue::Array(arr) => {
                let elems: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            JsValue::Function(func) => write!(f, "function({}) {{...}}", func.parameters.join(", ")),
        }
    }
}

/// Logs a sequence of JsValue instances to the console, similar to JavaScript's `console.log`.
pub fn console_log(values: Vec<JsValue>) {
    let output = values
        .iter()
        .map(|v| v.to_console_string())
        .collect::<Vec<String>>()
        .join(" ");
    println!("{}", output);
}