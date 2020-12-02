import hashlib
from collections import deque

KEY = 'ioramepc'

def md5_hex(key):
    return tuple(hashlib.md5(key.encode()).hexdigest()[:4])

def shortest_path(key, longest=False):
    longest_path = 0
    target = (6, 6)
    dists = {}
    queue = deque([((0, 0), '')])
    while queue:
        coord, path = queue.popleft()
        if coord == target:
            if not longest:
                return path
            longest_path = len(path)
            continue
        for d, i in zip(['U', 'D', 'L', 'R'], md5_hex(key + path)):
            if i in 'bcdef':
                x, y = coord
                a, b = {'U': (0, -2),
                        'D': (0, 2),
                        'L': (-2, 0),
                        'R': (2, 0)}[d]
                new = (x + a, y + b)
                if not new[0] in range(8) or not new[1] in range(8):
                    continue
                queue.append((new, path + d))
    return longest_path

def main():
    print('Part 1:', shortest_path(KEY))
    print('Part 2:', shortest_path(KEY, longest=True))

if __name__ == "__main__":
    main()
