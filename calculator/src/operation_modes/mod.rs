use std::io::{BufRead, Write};

use crate::operation_modes::error::CalculatorError;
use crate::Calculator;

pub mod decimal_place_creation;
pub mod error;
pub mod execution;
pub mod list_construction;
pub mod whole_number_creation;

/// Defines a type alias using our own error type.
type CalculatorResult<T> = std::result::Result<T, CalculatorError>;

/// Defines a common interface for the OperationModes. This makes it possible to use some kind of
/// polymorphism, to use the different operation modes.
pub(crate) trait OperationMode<R: BufRead, W: Write> {
    /// Takes a mutable reference of the Calculator and the first char from the command stream and
    /// processes it.
    fn read(&self, ctx: &mut Calculator<R, W>, input: char) -> CalculatorResult<()>;
}
