
grid = [[0 for i in range(1000)] for j in range(1000)]

for line in open('data/06.in'):
    line = line.split()
    if line[0] == 'toggle':
        x, y = map(int, line[1].split(','))
        a, b = map(int, line[3].split(','))
        for i in range(x, a + 1):
            for j in range(y, b + 1):
                grid[i][j] += 2
    else:
        new = {'on': 1, 'off': -1}[line[1]]
        x, y = map(int, line[2].split(','))
        a, b = map(int, line[4].split(','))
        for i in range(x, a + 1):
            for j in range(y, b + 1):
                if new == -1 and grid[i][j] == 0:
                    continue
                grid[i][j] += new

count = 0
for row in grid:
    for i in row:
        count += i

print(count)

# Attempts: 13396307 (Too low)

