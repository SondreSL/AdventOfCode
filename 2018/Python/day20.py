from pprint import pprint
from collections import deque

def read_file(filename):
    with open(filename) as f:
        return f.readline().strip()[1:]


def create_map(regex, coord=(0, 0), doors=set()):
    crossroad = coord
    while regex:
        curr = regex[0]
        if curr == ')':
            return regex[1:]
        elif curr == '(':
            regex = create_map(regex[1:], coord=coord, doors=doors)
            continue
        elif curr == '|':
            coord = crossroad
            regex = regex[1:]
            continue
        elif curr == 'N':
            new = (coord[0], coord[1] - 2)
        elif curr == 'W':
            new = (coord[0] - 2, coord[1])
        elif curr == 'E':
            new = (coord[0] + 2, coord[1])
        elif curr == 'S':
            new = (coord[0], coord[1] + 2)
        doors.add((tuple(sorted((coord, new)))))
        regex = regex[1:]
        coord = new
    return doors

def distances(doors):
    dist = {(0,0): 0}
    waiting = deque([(0,0)])
    while waiting:
        start = waiting.popleft()
        for k, v in (k for k in doors if start in k):
            if k == start and v not in dist:
                waiting.append(v)
                dist[v] = dist[start] + 1
            elif v == start and k not in dist:
                waiting.append(k)
                dist[k] = dist[start] + 1
    return dist

def draw_doors(doors):
    all_doors = set()
    for k, v in doors:
        all_doors.add(k)
        all_doors.add(v)
    minX = min(all_doors, key=lambda x: x[0])[0] - 1
    maxX = max(all_doors, key=lambda x: x[0])[0] + 2
    minY = min(all_doors, key=lambda x: x[1])[1] - 1
    maxY = max(all_doors, key=lambda x: x[1])[1] + 2
    for i in range(minY, maxY):
        for j in range(minX, maxX):
            if (j,i) == (0, 0):
                print('X', end='')
            elif (j,i) in all_doors:
                print('.', end='')
            elif ((j, i - 1), (j, i + 1)) in doors:
                print('-', end='')
            elif ((j - 1, i), (j + 1, i)) in doors:
                print('|', end='')
            else:
                print('#', end='')
        print()


def solveA(dists):
    print(f'Part 1: {max(dists.values())}')

def solveB(dists):
    print(f'Part 2: {sum(1 for k, v in dists.items() if v >= 1000)}')

def main():
    regex = read_file('input')
    doors = create_map(regex)
    dists = distances(doors)
    solveA(dists)
    solveB(dists)

if __name__ == "__main__":
    main()
