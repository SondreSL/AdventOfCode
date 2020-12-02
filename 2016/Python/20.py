import numpy as np
# 4294967295

def read_file(filename):
    data = []
    with open(filename) as f:
        for line in f:
            data.append(list(map(int, line.split('-'))))
    return sorted(data)


def solveA(lines):
    smallest = 0
    for a, b in zip(lines, lines[1:]):
        if b[0] in range(a[0], a[1] + 2):
            continue
        return a[1] + 1

def solveB(lines):
    MAX = 4294967295
    arr = np.ones(MAX + 1, dtype=np.bool_)
    for a, b in lines:
        arr[a:b + 1] = False
    return np.count_nonzero(arr)

def main():
    lines = read_file('input/20.in')
    print('Part 1:', solveA(lines))
    print('Part 2:', solveB(lines))

if __name__ == "__main__":
    main()

# 834840788 (Too high) 
