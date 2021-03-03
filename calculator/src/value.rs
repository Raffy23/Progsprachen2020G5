use std::fmt;

/// Value represents the data types the calculator should process.
/// - Single: Represents a single float value.
/// - List: Represents a list of anything, commands, single values, nested lists..
///
/// Value implements the Default trait, which is Single(0.0).
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Single(f64),
    List(String),
}

impl Default for Value {
    fn default() -> Self {
        Value::Single(0.0)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Single(v) => write!(f, "{}", v),
            Value::List(v) => write!(f, "{}", v),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        match value {
            true => Value::Single(1.0),
            false => Value::Single(0.0),
        }
    }
}

impl Value {
    pub fn is_single(&self) -> bool {
        match self {
            Value::Single(_) => true,
            Value::List(_) => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Value::Single(_) => false,
            Value::List(_) => true,
        }
    }

    /// Converts the `Value::Single` to a boolean based on an epsilon.
    pub fn to_bool(&self, epsilon: f64) -> bool {
        assert!(self.is_single());
        if let Value::Single(value) = self {
            value.abs() >= epsilon
        } else {
            unreachable!();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn display_value() {
        assert_eq!(format!("{}", Value::Single(1.25)), "1.25");
        assert_eq!(format!("{}", Value::Single(1.0)), "1");
        assert_eq!(format!("{}", Value::List("abc".to_string())), "abc");
    }

    #[test]
    fn to_bool() {
        assert_eq!(Value::Single(0.01).to_bool(0.1), false);
        assert_eq!(Value::Single(0.1).to_bool(0.1), true);
    }
}
