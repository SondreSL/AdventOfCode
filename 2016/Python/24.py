from collections import deque
from itertools import permutations

def read_file(filename):
    data = {}
    nums = {}
    with open(filename) as f:
        for y, line in enumerate(f):
            for x, cell in enumerate(line):
                if cell != '#':
                    data[(x, y)] = True
                if cell in '1234567890':
                    nums[int(cell)] = (x, y)
    return data, nums

def find_all_dists(data, nums):
    keys = sorted(nums.keys())
    return [[find_shortest_path(data, nums[k], nums[j]) for j in keys] for k in keys]

def find_shortest_path(data, start, end):
    seen = set()
    queue = deque([(start, 0)])
    while queue:
        coord, dist = queue.popleft()
        if coord == end:
            return dist
        x, y = coord
        for a, b in ((0, -1), (0, 1), (-1, 0), (1, 0)):
            new = (x + a, y + b)
            if new not in data or new in seen:
                continue
            seen.add(new)
            queue.append((new, dist + 1))

def generate_routes(n, circular):
    for perm in (list(i) for i in permutations(range(n)) if i[0] == 0):
        if circular:
            perm.append(0)
        yield zip(perm, perm[1:])

def find_shortest_route(data, nums, circular=False):
    dists = find_all_dists(data, nums)
    return min((sum(dists[a][b] for a, b in p)) for p in generate_routes(len(dists), circular))

def main():
    data, nums = read_file('input/24.in')
    print('Part 1:', find_shortest_route(data, nums))
    print('Part 2:', find_shortest_route(data, nums, circular=True))

if __name__ == "__main__":
    main()
