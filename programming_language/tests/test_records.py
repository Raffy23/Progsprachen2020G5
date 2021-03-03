import unittest
import interpreter
import logging


class TestStringMethods(unittest.TestCase):

    def setUp(self) -> None:
        logging.basicConfig(level=logging.DEBUG)

    def test_simple_records(self):
        value = interpreter.parse_string("{a=x->mult x x} a 4")
        self.assertEqual(value.value, 16)  # type: ignore

    def test_additional_records(self):
        self.assertEqual(interpreter.parse_string(
            "{x=5} sub x 1").value, 4)  # type: ignore

    def test_complex_records(self):
        self.assertEqual(interpreter.parse_string(
            "{a=x->y->add(mult x x)y, b=a 2, c=b 3} sub(b 5)c").value, 2)  # type: ignore

    def test_complex_records_nested(self):
        self.assertEqual(interpreter.parse_string("""{
        append = x1->y1->cond x1 {head=x1 head, tail=append(x1 tail)y1} y1,
        gen = x2->cond x2 (append (gen(sub x2 1)) {head=x2, tail={}}) {}
        }
        gen 3""").__str__(), '{head=1, tail={head=2, tail={head=3, tail={}}}}')  # type: ignore


if __name__ == "__main__":
    unittest.main()
