import re

def read_file(filename):
    with open(filename) as f:
        triangles = []
        for line in f:
            triangles.append(tuple(int(i) for i in re.findall(r'\d+', line)))
    return triangles

def legal_triangle(a, b, c):
    return a + b > c and a + c > b and b + c > a

def solveA(triangles):
    return sum(legal_triangle(*a) for a in triangles)

def solveB(triangles):
    ts = iter(triangles)
    count = 0
    for a in ts:
        b = next(ts)
        c = next(ts)
        count += sum(legal_triangle(*i) for i in zip(a, b, c))
    return count

def main():
    triangles = read_file('input/03.in')
    print('Part 1:', solveA(triangles))
    print('Part 2:', solveB(triangles))

if __name__ == "__main__":
    main()

# Attempts: 1067 (Too low)
