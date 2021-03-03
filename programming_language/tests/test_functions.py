import unittest
import interpreter


class TestStringMethods(unittest.TestCase):

    def test_simple_expression(self):
        self.assertEqual(interpreter.parse_string(
            "add (mult 40 50) 3050").value, 5050)  # type: ignore

    def test_complex_function(self):
        self.assertEqual(interpreter.parse_string(
            "((x->(y->add(mult x x)y))2)3").value, 7)  # type: ignore

    def test_simple_function(self):
        self.assertEqual(interpreter.parse_string(
            "(x->mult x x) 32").value, 1024)  # type: ignore

    def test_complex_function_without_parantheses(self):
        self.assertEqual(interpreter.parse_string(
            "(x->y->add(mult x x)y) 2 3").value, 7)  # type: ignore
