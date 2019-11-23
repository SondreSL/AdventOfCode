from collections import Counter

def read_file(filename):
    return open(filename).read().splitlines()

def solve(lines):
    lines = list(map(list, zip(*lines)))
    first = ''
    last = ''
    for line in lines:
        count = Counter(line).most_common()
        first += count[0][0]
        last += count[-1][0]
    print('Part 1:', first)
    print('Part 2:', last)

def main():
    lines = read_file('input/06.in')
    solve(lines)

if __name__ == "__main__":
    main()
