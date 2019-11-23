from collections import defaultdict

def read_file(filename):
    with open(filename) as f:
        grid = []
        for line in f:
            temp = []
            for cell in line.strip():
                temp.append(True) if cell == '#' else temp.append(False)
            grid.append(temp)
        return grid

def gen_coords(grid, lock=[]):
    yield from ((x,y) for y in range(len(grid)) for x in range(len(grid[y])) if (x,y) not in lock)

def move(grid, lock=[]):
    to_die = set()
    to_live = set()
    for x, y in gen_coords(grid, lock=lock):
        nbs = count_nbs((x,y), grid)
        if grid[y][x] and nbs not in [2,3]:
            to_die.add((x,y))
        elif nbs == 3:
            to_live.add((x,y))

    for x, y in to_die:
        grid[y][x] = False
    for x, y in to_live:
        grid[y][x] = True


def count_nbs(coord, grid):
    a, b = coord
    count = 0
    for y in (-1, 0, 1):
        for x in (-1, 0, 1):
            if x == y == 0:
                continue
            if 0 > a + x or a + x >= len(grid[0]):
                continue
            if 0 > b + y or b + y >= len(grid):
                continue
            if grid[b+y][a+x]:
                count += 1
    return count

def count_lights(grid):
    return sum(1 for x, y in gen_coords(grid) if grid[y][x])

def draw_grid(grid):
    for line in grid:
        for cell in line:
            sign = {True: '#', False: '.'}[cell]
            print(sign, end='')
        print()
    print()

def simulate(grid, generations=100, lock=[]):
    m = len(grid) - 1
    for x, y in lock:
        grid[y][x] = True
    for _ in range(generations):
        move(grid, lock)
    return count_lights(grid)

def solveB(grid):
    # Lock the corners to be True
    m = len(grid) - 1
    return simulate(grid, lock=[(0,0), (0,m), (m,0), (m,m)])


def main():
    grid = read_file('18.in')
    print('Part 1:', simulate(grid))
    grid = read_file('18.in')
    print('Part 2:', solveB(grid))

if __name__ == "__main__":
    main()
