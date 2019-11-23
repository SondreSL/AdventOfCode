from dataclasses import dataclass
import re

class FoundDiff(Exception):
    pass

def read_file(filename):
    nodes = {}
    with open(filename) as f:
        for line in f:
            weight = int(re.findall(r'\d+', line)[0])
            names = re.findall(r'[a-zA-Z]+', line)
            name = names[0]
            if len(names) > 1:
                nodes[name] = (weight, tuple(names[1:]))
            else:
                nodes[name] = (weight, tuple())
    return nodes

def find_root(nodes):
    root = list(nodes)[0]
    while True:
        for n in nodes.keys():
            if root in nodes[n][1]:
                root = n
                break
        else:
            return root

def find_wrong_weight(nodes):
    root = find_root(nodes)
    ws = [find_weight(c, nodes) for c in nodes[root][1]]
    print(ws)
    return max(ws) - min(ws)

def find_weight(n, nodes):
    ws = [find_weight(c, nodes) for c in nodes[n][1]]
    return sum(ws) + nodes[n][0]

def main():
    nodes = read_file('input/07.in')
    print('Part 1:', find_root(nodes))
    print('Part 2:', find_wrong_weight(nodes))


if __name__ == "__main__":
    main()

# 9
# 42
# 6959
