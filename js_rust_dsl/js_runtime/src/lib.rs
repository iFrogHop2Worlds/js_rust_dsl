use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
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
            (self_val, JsValue::String(s)) => {
                JsValue::String(format!("{}{}", self_val.to_string(), s))
            }
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
            _ => false, // Default for objects, arrays
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
            JsValue::Array(_) => write!(f, "[array]"),       // Simplified for now
        }
    }
}

pub fn console_log(value: &JsValue) {
    println!("{}", value);
}