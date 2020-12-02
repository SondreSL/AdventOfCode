"""Advent of Code 2016 Day 9"""
import re

def read_file(filename):
    return open(filename).read().strip()

def decompress(string, part2=False):
    new = ''
    while string:
        if string[0] != '(':
            new += string[0]
            string = string[1:]
            continue
        index = string.find(')')
        code = string[1:index]
        chars, repeat = [int(i) for i in code.split('x')]
        string = string[index + 1:]
        substring = string[:chars]
        if part2:
            while re.findall(r'\(\d+x\d+\)', substring):
                substring = decompress(substring, part2=True)
        string = string[chars:]
        new += substring * repeat
    return new

def main():
    line = read_file('input/09.in')
    print('Part 1:', len(decompress(line)))
    print('Part 2:', len(decompress(line, part2=True)))

if __name__ == "__main__":
    main()
