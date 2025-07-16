use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub enum JsValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
    Object(HashMap<String, JsValue>),
    Array(Vec<JsValue>),
    // Function(Box<dyn Fn(...) -> JsValue>), // todo
}

impl JsValue {
    /// Implements JavaScript's addition operator.
    /// Handles numeric addition and string concatenation.
    pub fn add(&self, other: &JsValue) -> JsValue {
        match (self, other) {
            (JsValue::Number(a), JsValue::Number(b)) => JsValue::Number(a + b),
            (JsValue::String(s), other) => JsValue::String(format!("{}{}", s, other.to_string())),
            (self_val, JsValue::String(s)) => JsValue::String(format!("{}{}", self_val.to_string(), s)),
            (a, b) => {
                let a_num = a.to_number();
                let b_num = b.to_number();
                if a_num.is_nan() | b_num.is_nan() {
                    JsValue::String(format!("{}{}", a.to_string(), b.to_string()))
                } else {
                    JsValue::Number(a_num + b_num)
                }
            }
        }
    }
    
    pub fn to_bool(&self) -> bool {
        match self {
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => *n!= 0.0 &&!n.is_nan(),
            JsValue::String(s) =>!s.is_empty(),
            JsValue::Null | JsValue::Undefined => false,
            JsValue::Object(_) | JsValue::Array(_) => true,
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
            JsValue::String(s) => write!(f, "{}", s),
            JsValue::Boolean(b) => write!(f, "{}", b),
            JsValue::Null => write!(f, "null"),
            JsValue::Undefined => write!(f, "undefined"),
            JsValue::Object(_) => write!(f, "[object Object]"), // Simplified for now
            JsValue::Array(_) => write!(f, "[array]"), // Simplified for now
        }
    }
}

pub fn console_log(value: &JsValue) {
    println!("{}", value);
}