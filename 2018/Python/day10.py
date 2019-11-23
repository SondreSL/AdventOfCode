"""Advent of Code 2018 Day 10"""
from pprint import pprint

def read_file(filename):
    points = []
    with open(filename) as f:
        for line in f:
            x, y, dx, dy = (int(i) for i in line.split())
            points.append((x, y, dx, dy))
    return points


def draw(left, right, up, down, points):
    points = set((x, y) for x, y, _, _ in points)
    for i in range(up, down + 1):
        for j in range(left, right + 1):
            if (j, i) in points:
                print('#', end='')
            else:
                print(' ', end='')
        print()

def move(points):
    new = []
    for x, y, dx, dy in points:
        new.append((x + dx, y + dy, dx, dy))
    return new

def set_extremes(points):
    left = min(points, key=lambda x: x[0])[0]
    right = max(points, key=lambda x: x[0])[0]
    up = min(points, key=lambda x: x[1])[1]
    down = max(points, key=lambda x: x[1])[1]
    return left, right, up, down

def main(filename):
    points = read_file(filename)

    left, right, up, down = set_extremes(points)
    dispersion = (right - left) * (down - up)
    new_disp = dispersion

    count = -1

    while new_disp <= dispersion:
        count += 1
        old = points
        dispersion = new_disp

        points = move(points)
        left, right, up, down = set_extremes(points)

        new_disp = (right - left) * (down - up)
    
    left, right, up, down = set_extremes(old)
    draw(left, right, up, down, old)
    print("Iterations:", count)

if __name__ == "__main__":
    main('day10.txt')
