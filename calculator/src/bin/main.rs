use std::io::{stderr, stdin};

use calculator::value::Value;
use calculator::StdCalculator;

/// Default program for calculator.
///
/// Program asks for input and on `return` executing it.
///
/// Example: Approximation of the surface area of a circle with radius=5:
/// ```text
/// > 22 7/Pp
/// 3.142857142857143
/// > 5p5**
/// 78.57142857142857
/// ```
fn default_program() -> String {
    "((> )\"'(< )\"@\"(\n)\")A(a@2!\\)2!@".to_string()
}

/// Calculates the surface area of a triangle in three-dimensional space.
///
/// 1. Get the coordinates from user.
///   a. First ask for the coordinates: A_X,A_Y,A_Z,B_X,B_Y,B_Z,C_X,C_Y,C_Z
///   b. Save them in registers: F,G,H,I,J,K,L,M,N
/// 2. Calculate vectors AB and AC.
///   a. AB is saved in O,P,Q
///   b. AC is saved in R,S,T
/// 3. Calculate  0.5 * |AB x AC|.
///   a. (p*t)-(s*q) -> U
///   b. (q*r)-(o*t) -> V
///   c. (o*s)-(r*p) -> W
///   d. sqrt(u^2+v^2+w^2) / 2
///
/// Example:
/// ```text
/// > 0 0 0 0 0 5 0 3 0b@
/// < 7.5
/// ```
fn surface_triangle_program() -> String {
    "NMLKJIHGFif-Ojg-Pkh-Qlf-Rmg-Snh-Tpt*sq*-Uqr*ot*-Vos*rp*-Wuu*vv*ww*++_2/".to_string()
}

/// Calculates the surface area of a regular **octahedron** twice. First the formula used for the
/// first solution is $A = 2*\sqrt(3)*a^2$. Second the result is calculated by reusing the
/// `surface_triangle_program()`. This uses the 'Pythagorean theorem' to calculate the coordinates
/// of a single triangle.
///
/// 1. Get the edge length *a* from the user and save it into register *F*.
/// 2. Calculate the surface area of the octahedron using the formula from above.
/// 3. Calculate the coordinates of a single triangle of an octahedron and put them on the stack.
/// 4. Execute the `surface_triangle_program()`.
///
/// Example:
/// ```text
/// > 5c@
/// < 86.60254037844386
///   86.60254037844388
/// ```
fn octahedron_program() -> String {
    "Fff*3_*2*\"(\n  )\"f2/E0 0 0 f 0 0 e ff*ee*-_ 0b@8*".to_string()
}

/// Calculates the sum of a given number of triangles.
///
/// 1. Copies the number of triangles to register 'E'
/// 2. Sets register 'Z' to 0.
/// 3. Copies the function to calculate the 'surface of a single triangle into register 'Y'.
/// 4. Copies the function for executing 'Y' in a while-loop into 'X' and executes it.
///
/// Example:
/// ```text
/// > 0 0 0 0 0 5 0 3 0 1 1 1 1 1 6 1 4 1 2d@
/// < 15
/// ```
fn sum_triangle_surface_program() -> String {
    "E0Z(9!9$9!9$9!9$9!9$9!9$9!9$9!9$9!9$9!9$b@z+Z)Y((z)(e1-Ey@x@)(e0>1+$@)@)Xx@".to_string()
}

/// ## Program-Register's
/// - A: `default_program()`
/// - B: `surface_triangle_program()`
/// - C: `octahedron_program()`
/// - D: `sum_triangle_surface()`
///
/// ## Used registers in different program's
/// - `default_program()`: A
/// - `surface_triangle_program()`: F-W
/// - `octahedron_program()`: E-F + `surface_triangle_program()`
/// - `sum_triangle_surface_program()`: E,X-Z + `surface_triangle_program()`
fn main() {
    let app = clap::App::new("Programming languages: Group 5 - Assignment 1")
        .author("Maximilian Schoenenberg <e11931241@student.tuwien.ac.at>")
        .about("A programmable stack-based calculator.")
        .long_about(
            "A programmable stack based calculator.\n\n\
        When started the default program is loaded, which is a basic command \
        prompt executing your command on every return.\n\n\
        The following example programs are included in this calculator:\n\
          - Calculation of the surface area of a 3D-triangle                    -- Register B\n\
          - Calculation of the surface area of an octahedron                    -- Register C\n\
          - Calculation of the surface area of an arbitrary number of triangles -- Register D\n\n\
        EXAMPLE:\n\
        > 5 3 +\n\
        < 8\n\
        > 0 0 0 0 0 5 0 3 0b@\n\
        < 7.5\n\
        > 5c@\n\
        < 86.60254037844386\n\
          86.60254037844388\n\
        > 0 0 0 0 0 5 0 3 0 1 1 1 1 1 6 1 4 1 2d@\n\
        < 15",
        )
        .arg(
            clap::Arg::new("debug")
                .short('d')
                .long("debug")
                .about("Turns on debug mode")
                .long_about(
                    "Turns on debug mode, which prints after each execution step the current \
                command, the command-stream and the data-stack",
                )
                .takes_value(false),
        )
        .get_matches();

    // Getting handles from STDIN and STDERR. Why STDERR and not STDOUT? STDERR has no line
    // buffering and therefore we don't need to print '\n' every time we output something.
    let input_stream = stdin();
    let output_stream = stderr();

    // Constructing the default calculator using the convenience constructor
    let mut calc = StdCalculator::new(
        input_stream.lock(),
        output_stream,
        default_program(),
        0.00000000000001,
    );

    // The example programs have to be added.
    calc.register_set[1] = Value::List(surface_triangle_program());
    calc.register_set[2] = Value::List(octahedron_program());
    calc.register_set[3] = Value::List(sum_triangle_surface_program());

    // Start the calculator, by executing its run loop
    calc.run(app.is_present("debug"));
}

#[cfg(test)]
mod test {
    use super::*;
    use calculator::TestCalculator;

    /// Testing `surface_triangle_program()` with edge length 5.
    #[test]
    fn octahedron() {
        let mut calc = TestCalculator::from(("5c@", Value::Single(0.0)));
        calc.register_set[1] = Value::List(surface_triangle_program());
        calc.register_set[2] = Value::List(octahedron_program());
        calc.register_set[3] = Value::List(sum_triangle_surface_program());

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(86.60254037844388));
    }

    /// Testing `surface_triangle_program()` with 1 triangle.
    #[test]
    fn surface_triangle() {
        let mut calc = TestCalculator::from(("0 0 0 0 0 5 0 3 0b@1.0", Value::Single(0.0)));
        calc.register_set[1] = Value::List(surface_triangle_program());
        calc.register_set[2] = Value::List(octahedron_program());
        calc.register_set[3] = Value::List(sum_triangle_surface_program());

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));
    }

    /// Testing `sum_triangle_surface_program()` with 0 triangles.
    #[test]
    fn sum_triangle_surface_0() {
        let mut calc = TestCalculator::from(("0d@0=", Value::Single(0.0)));
        calc.register_set[1] = Value::List(surface_triangle_program());
        calc.register_set[2] = Value::List(octahedron_program());
        calc.register_set[3] = Value::List(sum_triangle_surface_program());

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));
    }

    /// Testing `sum_triangle_surface_program()` with 1 triangles.
    #[test]
    fn sum_triangle_surface_1() {
        let mut calc = TestCalculator::from(("0 0 0 0 0 5 0 3 0 1d@7.5=", Value::Single(0.0)));
        calc.register_set[1] = Value::List(surface_triangle_program());
        calc.register_set[2] = Value::List(octahedron_program());
        calc.register_set[3] = Value::List(sum_triangle_surface_program());

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));
    }

    /// Testing `sum_triangle_surface_program()` with 3 triangles.
    #[test]
    fn sum_triangle_surface_3() {
        let mut calc = TestCalculator::from((
            "0 0 0 0 0 5 0 3 0 1 1 1 1 1 6 1 4 1 2 2 2 2 2 7 2 5 2 3d@22.5=",
            Value::Single(0.0),
        ));
        calc.register_set[1] = Value::List(surface_triangle_program());
        calc.register_set[2] = Value::List(octahedron_program());
        calc.register_set[3] = Value::List(sum_triangle_surface_program());

        calc.run(false);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));
    }
}
