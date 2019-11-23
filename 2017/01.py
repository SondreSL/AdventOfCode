"""Advent of Code 2017 Day 1."""

def find_captcha(line):
    line = list(line)
    line.append(line[0])
    return sum(a for a, b in zip(line, line[1:]) if a == b)

def solveB(line):
    l = len(line) // 2
    total = 0
    for i, a in enumerate(line):
        if a == (line[(i + l) % len(line)]):
            total += a
    return total

def main():
    line = list(int(i) for i in list(open('input/01.in').readline().strip()))
    print('Part 1:', find_captcha(line))
    print('Part 2:', solveB(line))

if __name__ == "__main__":
    main()

# 1081 (Too low)
