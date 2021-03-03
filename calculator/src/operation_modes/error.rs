use std::fmt;

/// A calculator error.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CalculatorError {
    pub(crate) error_type: CalculatorErrorType,
}

/// Defines types of calculator errors.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CalculatorErrorType {
    WrongOperationMode(i8),
    UnexpectedDataTypeOnDataStack,
    EmptyDataStack,
    NotEnoughElementsOnTheDataStack,
    UndefinedCommand(char),
    IOError(Box<String>),
}

impl CalculatorError {
    /// Constructs an error from a given CalculatorErrorType
    pub fn from_type(error_type: CalculatorErrorType) -> Self {
        CalculatorError { error_type }
    }
}

impl fmt::Display for CalculatorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.error_type)
    }
}
