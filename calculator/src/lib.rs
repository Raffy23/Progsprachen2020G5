pub mod operation_modes;
pub mod value;

use std::collections::vec_deque::VecDeque;
use std::io::{BufRead, StdinLock, Write};

use crate::operation_modes::{
    decimal_place_creation::DecimalPlaceConstructionMode, execution::ExecutionMode,
    list_construction::ListConstructionMode, whole_number_creation::WholeNumberConstructionMode,
    OperationMode,
};
use crate::value::Value;

/// Calculator owns all parts of the calculator and provides the run method, to execute it.
/// The parts of the calculator are:
/// - Command stream: Implemented with a VecDeque, which is a in-fact a double ended queue,
///   for efficient inserting on both ends. This is especially useful for the operations *Apply
///   immediately* and *Apply later*.
/// - Operation mode: Is implemented with a 8-bit signed Integer. This limits the number of decimal
///   places naturally.
/// - Data stack: Uses a Vec data structure, which is recommended for using it as a stack. See
///   [Use a Vec when?](https://doc.rust-lang.org/std/collections/#use-a-vec-when).
/// - Register set: Is a fixed-size array, which can store a Value.
/// - Input stream: The input stream is a template parameter, to make testing possible. It has to
///   conform to the trait BufRead.
/// - Output stream: The output stream is aswell a template parameter, to make testing possible. It
///   has to conform to the trait Write.
/// - Epsilon: A configurable floating-point error.
pub struct Calculator<R, W> {
    pub command_stream: VecDeque<char>,
    pub operation_mode: i8,
    pub stack: Vec<Value>,
    pub register_set: [Value; 26],
    pub input: R,
    pub output: W,
    pub epsilon: f64,
}

impl<R, W> Calculator<R, W>
where
    R: BufRead,
    W: Write,
{
    /// The run loop of the calculator, which reads from the command stream, char by char, and
    /// executes it using the determined OperationMode.
    ///
    /// It terminates when the command stream is empty.
    pub fn run(&mut self, debug_mode: bool) {
        // Init calculator by reading from register a. So we push the instruction to read from a
        // and execute it afterwards onto the stack, so we do not have to reinvent the wheel here.
        //
        // Remark: For testing purposes, we do not require the program in register 'a'. This
        // expression is evaluated at compile time, so it is no overhead at runtime.
        if cfg!(test) && self.register_set[0] == Value::Single(0.0) {
            println!("Test mode: No init from register");
        } else {
            self.command_stream.push_back('a');
            self.command_stream.push_back('@');
        }

        while let Some(cmd) = self.command_stream.pop_front() {
            let mode = Self::determine_mode(self.operation_mode);

            // Debug part
            if debug_mode {
                let c: String = if cmd == '\n' {
                    "\\n".to_string()
                } else {
                    cmd.to_string()
                };
                let s: String = self
                    .command_stream
                    .iter()
                    .map(|&c| {
                        if c == '\n' {
                            "\\n".to_string()
                        } else {
                            c.to_string()
                        }
                    })
                    .collect();
                println!("cmd:'{}' -- stream:{} -- stack:{:?}", c, s, self.stack);
            }

            match mode.read(self, cmd) {
                Ok(_) => {
                    // Do nothing, just continue processing...
                }
                Err(err) => {
                    panic!("Error while executing command '{}': {}", cmd, err);
                }
            }
        }
    }

    /// Determines the operation mode and returns a reference to the instantiated `OperationMode`
    /// struct.
    ///
    /// This was benchmarked against a heap-based solution and solution with reusing the objects,
    /// but this was the fastest one (see this
    /// [benchmark](https://gist.github.com/schoenenberg/1e03d1f8e412ba5323073192798c8215)).
    fn determine_mode<'a>(mode: i8) -> &'a dyn OperationMode<R, W> {
        match mode {
            _ if mode > 0 => &ListConstructionMode {},
            0 => &ExecutionMode {},
            -1 => &WholeNumberConstructionMode {},
            _ if mode < -1 => &DecimalPlaceConstructionMode {},
            _ => unreachable!(),
        }
    }
}

/// Is a type alias for the generic Calculator using STDIN and either STDOUT or STDERR as in- and
/// output stream.
pub type StdCalculator<'a, O> = Calculator<StdinLock<'a>, O>;

impl<'a, O> StdCalculator<'a, O> {
    pub fn new(
        stdin_stream: StdinLock<'a>,
        output_stream: O,
        init_program: String,
        epsilon: f64,
    ) -> Self {
        let mut register_set: [Value; 26] = Default::default();
        register_set[0] = Value::List(init_program);

        StdCalculator {
            command_stream: VecDeque::new(),
            operation_mode: 0,
            stack: vec![Value::Single(0.0)], // Init with zero
            register_set,
            input: stdin_stream,
            output: output_stream,
            epsilon,
        }
    }
}

/// Is a type alias for the calculator, only used for testing. It provides an implementation of the
/// Default trait, for easy instantiation. Implementation of the in- and output stream are a static
/// byte slice and a Vec of bytes.
pub type TestCalculator = Calculator<&'static [u8], Vec<u8>>;

impl Default for TestCalculator {
    fn default() -> Self {
        Self {
            command_stream: VecDeque::new(),
            operation_mode: 0,
            stack: vec![],
            register_set: Default::default(),
            input: b"",
            output: Vec::new(),
            epsilon: 0.00000000000001,
        }
    }
}

impl From<(&str, Value)> for TestCalculator {
    fn from((init_program, value): (&str, Value)) -> Self {
        let mut calc = Self::default();
        calc.register_set[0] = Value::List(init_program.to_string());
        calc.stack.push(value);

        calc
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::iter::FromIterator;

    /// Example from page 6 of the assignment.
    #[test]
    fn conditional_execution_false_path() {
        let mut calc = TestCalculator::default();
        calc.stack.push(Value::Single(0.0));
        calc.command_stream = VecDeque::from_iter("(9~)(8)(4!4$1+$@)@".chars());

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(-9.0));
    }

    /// Example from page 6 of the assignment, but initialised with true.
    #[test]
    fn conditional_execution_true_path() {
        let mut calc = TestCalculator::from(("(9~)(8)(4!4$1+$@)@", Value::Single(1.0)));

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(8.0));
    }

    /// Factorial example from page 8 of the assignment.
    #[test]
    fn factorial_calculator() {
        let mut calc = TestCalculator::from((
            "(3!3!1-2!1=4!()(4!4$1+$@)@2$*)3!3$3!@2$",
            Value::Single(3.0),
        ));

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(6.0));
    }
}
