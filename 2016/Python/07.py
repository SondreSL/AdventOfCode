import re

def read_file(filename):
    return open(filename).read().splitlines()

def supports_tls(lines):
    for line in lines:
        abba = re.split(r'\[[a-zA-Z]*\]', line)
        if any(is_abba(a) for a in abba):
            yield line

def supports_ssl(line):
    ssl = re.split(r'\[[a-zA-Z]*\]', line)
    candidates = set()
    for sub in ssl:
        for a, b, c in zip(sub, sub[1:], sub[2:]):
            if a == c and a != b:
                candidates.add(''.join([a,b,c]))
    inside = re.findall(r'\[([a-zA-Z]+)\]', line)
    for sub in inside:
        for a,b,c in candidates:
            if ''.join([b,a,b]) in sub:
                return True
    return False

def is_abba(line):
    for a, b, c, d in zip(line, line[1:], line[2:], line[3:]):
        if a == d and b == c and a != b:
            return True
    return False

def solveA(lines):
    count = 0
    for line in supports_tls(lines):
        inside = re.findall(r'\[([a-zA-Z]+)\]', line)
        if not any(is_abba(i) for i in inside):
            count += 1
    return count

def solveB(lines):
    return sum(supports_ssl(l) for l in lines)


def main():
    lines = read_file('input/07.in')
    print('Part 1:', solveA(lines))
    print('Part 2:', solveB(lines))

if __name__ == "__main__":
    main()

# 81
# 117 (Too high)
