from itertools import cycle
from collections import deque
from collections import defaultdict
import re

def read_file(filename):
    with open(filename) as f:
        data = f.read().splitlines()
        return deque(data)

def solve(queue):
    data = defaultdict(list)
    output = {}
    while queue:
        line = queue.popleft()
        parts = line.split()
        if parts[0] == 'value':
            val, bot = (int(i) for i in re.findall(r'\d+', line))
            data[bot].append(val)
        else:
            bot, k1, v1, k2, v2 = re.findall(r'bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)', line)[0]
            bot, v1, v2 = int(bot), int(v1), int(v2)
            if len(data[bot]) != 2:
                 queue.append(line)
                 continue
            if k1 == 'bot':
                data[v1].append(min(data[bot]))
            else:
                output[v1] = min(data[bot])
            if k2 == 'bot':
                data[v2].append(max(data[bot]))
            else:
                output[v2] = max(data[bot])
    for k, v in data.items():
        if sorted(v) == [17, 61]:
            return k, output[0] * output[1] * output[2]

def main():
    lines = read_file('input/10.in')
    part1, part2 = solve(lines)
    print('Part 1:', part1)
    print('Part 2:', part2)

if __name__ == "__main__":
    main()

# 107 (Too low)
