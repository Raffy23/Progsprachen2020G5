#!/usr/bin/env python3
import sys, logging
from lexer import tokenize
from parsing import parse

logger = logging.getLogger(__name__)

def read_file(filename) -> str:
    with open(filename, "r") as file:
        contents = file.read()
    return contents

def parse_string(contents: str):
    logger.debug('Input code: %s', contents)
    tokens = tokenize(contents)
    return parse(tokens) 

def print_help():
    print(f'Usage: python3 {sys.argv[0]} /path/to/code')


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Invalid arguments")
        print_help()
        sys.exit(1)
    logging.basicConfig(level=logging.INFO)
    print("---- Starting interpreter v1.0 ----\n")
    contents = read_file(sys.argv[1])
    result = parse_string(contents)
    print(result)
