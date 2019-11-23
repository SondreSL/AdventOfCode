
def read_file(filename):
    with open(filename) as f:
        for line in f:
            line = line.split()
            if line[0] == 'rect':
                yield [int(i) for i in line[1].split('x')]
            elif line[1] == 'row':
                move = line[1]
                _, i = line[2].split('=')
                i = int(i)
                num = int(line[4])
                yield move, i, num
            else:
                move = line[1]
                _, i = line[2].split('=')
                i = int(i)
                num = int(line[4])
                yield move, i, num

def change_grid(grid, cmd):
    if len(cmd) == 2:
        a, b = cmd
        for i in range(b):
            for j in range(a):
                grid[i][j] = True
    else:
        move, a, b = cmd
        if move == 'row':
            copy = grid[a][:]
            for i, val in enumerate(copy):
                grid[a][(i + b) % len(grid[a])] = val
        else:
            copy = [grid[i][a] for i in range(len(grid))]
            for i, val in enumerate(copy):
                grid[(i + b) % len(grid)][a] = val

def solveA(grid):
    return sum(1 for line in grid for c in line if c)

def draw(grid):
    for line in grid:
        for c in line:
            if c:
                print('#', end='')
            else:
                print(' ', end='')
        print()

def main():
    grid = [[False for _ in range(50)] for _ in range(6)]
    for line in read_file('input/08.in'):
        change_grid(grid, line)
    print('Part 1:', solveA(grid))
    draw(grid) # Look at output to find answer

if __name__ == "__main__":
    main()
