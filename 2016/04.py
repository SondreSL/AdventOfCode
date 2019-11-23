from string import ascii_lowercase
from collections import Counter
import re

def read_file(filename):
    return open(filename).read().splitlines()

def valid(line):
    num, check = re.findall(r'(\d+)\[([a-zA-Z]+)\]', line)[0]
    line = line.split('-')[:-1]
    count = Counter(''.join(line)).most_common()
    count.sort(key=lambda x: (x[1], -ord(x[0])), reverse=True)
    return ''.join(i for i, _ in count[:5]) == check

def valid_codes(lines):
    yield from (i for i in lines if valid(i))

def value(line):
    return int(re.findall(r'\d+', line)[0])

def shift_line(line):
    new = ''
    num = int(re.findall(r'\d+', line)[0])
    for c in ' '.join(line.split('-')[:-1]):
        if c == ' ':
            new +=  ' '
        else:
            i = ascii_lowercase.index(c)
            i = (i + num) % 26
            new += ascii_lowercase[i]
    return new, num

def solveA(lines):
    return sum(value(l) for l in valid_codes(lines))

def solveB(lines):
    for line in valid_codes(lines):
        code, num = shift_line(line)
        if 'northpole' in code:
            return num

def main():
    lines = read_file('input/04.in')
    # lines = read_file('test')
    print('Part 1:', solveA(lines))
    print('Part 2:', solveB(lines))

if __name__ == "__main__":
    main()

# Attempts: 408259 (Too low)
