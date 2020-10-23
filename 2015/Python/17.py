from itertools import combinations

with open('../data/17.in') as f:
    CONTAINERS = [int(i) for i in f.read().splitlines()]

def find_count(part2=False):
    count = 0
    for i in range(1, len(CONTAINERS)):
        for c in combinations(CONTAINERS, i):
            if sum(c) == 150:
                count += 1
        if part2 and count > 0:
            return count
    return count


print('Part 1:', find_count())
print('Part 2:', find_count(part2=True))

# 1638
# 17
