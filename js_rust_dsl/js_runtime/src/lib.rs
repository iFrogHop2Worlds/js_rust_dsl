use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub struct JsFunction {
    pub parameters: Vec<String>,
    pub body: String, // kepping this for display purposes rn
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
                format!("function({}) {{ {} }}", func.parameters.join(", "), func.body)
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