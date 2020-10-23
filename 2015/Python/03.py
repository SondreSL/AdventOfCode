from collections import defaultdict

with open('03.in') as f:
    line = f.read().strip()

def move(pos, c):
    x, y = pos
    if c == '>':
        return (x + 1, y)
    if c == '<':
        return (x - 1, y)
    if c == '^':
        return (x, y - 1)
    if c == 'v':
        return (x, y + 1)

houses = defaultdict(int)
pos = (0, 0)
robo_pos = (0, 0)
houses[pos] += 1
houses[robo_pos] += 1
line = iter(line)
for c in line:
    pos = move(pos, c)
    houses[pos] += 1
    robo_pos = move(robo_pos, next(line))
    houses[robo_pos] += 1

print(len(houses))
