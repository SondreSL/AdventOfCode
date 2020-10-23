from ast import literal_eval
from itertools import zip_longest
from subprocess import check_output
import sys

def get_code(filename):
    l, c = map(int, check_output(['wc', '-l', '-c', filename]).split()[:2])
    return c - l

def encode(string):
    string = repr(string)
    chars = 2
    for a, b in zip_longest(string, string[1:]):
        if a == '"':
            chars += 2
        else:
            chars += 1

def solveA(filename):
    lines = open(filename).read().splitlines()
    chars = sum(len(literal_eval(line)) for line in lines)
    code = get_code(filename)
    print('Part 1:', code - chars)
    return code - chars

def solveB(filename):
    lines = open(filename).read().splitlines()
    ext_code = sum(encode(s) for s in lines)
    code = get_code(filename)
    print('Part 2:', ext_code - code)
    return ext_code - code

def main():
    filename = '08.in'
    solveA(filename)

if __name__ == "__main__":
    main()
