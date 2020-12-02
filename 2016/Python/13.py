from collections import deque

KEY = 1352

def space(key, loc):
    x, y = loc
    num = x*x + 3*x + 2*x*y + y + y*y + key
    num = str(bin(num)).count('1')
    return num % 2 == 0

def shortest_path(key, target=None, steps=0):
    queue = deque([((1,1), 0)])
    data = {}
    while queue:
        loc, dist = queue.popleft()
        if target is not None and loc == target:
            return dist
        data[loc] = True
        for x, y in ((-1, 0), (1, 0), (0, -1), (0, 1)):
            new = (loc[0] + x, loc[1] + y)
            if new[0] < 0 or new[1] < 0:
                continue
            if new in data or not space(key, new):
                continue
            if steps and dist >= steps:
                continue
            queue.append((new, dist + 1))
    return len(data)

def main():
    print('Part 1:', shortest_path(KEY, target=(31, 39)))
    print('Part 2:', shortest_path(KEY, steps=50))

if __name__ == "__main__":
    main()
