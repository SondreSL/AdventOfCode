from dataclasses import dataclass
import re

@dataclass
class Node:
    x: int
    y: int
    size: int
    used: int
    avail: int
    use_percent: int

def read_file(filename):
    nodes = {}
    with open(filename) as f:
        for line in f:
            x, y, size, used, avail, use_percent = list(map(int, re.findall(r'\d+', line)))
            nodes[(x, y)] = Node(x, y, size, used, avail, use_percent)
    return nodes

def is_viable(node, nodes):
    if node.used == 0:
        return False
    for comp in nodes.values():
        if comp == node:
            continue
        if node.used <= comp.avail:
            return True
    return False


"""
    Node A is not empty (its Used is not zero).
    Nodes A and B are not the same node.
    The data on node A (its Used) would fit on node B (its Avail).
"""

def find_viable_pairs(nodes):
    return sum(is_viable(node, nodes) for node in nodes.values())

def main():
    nodes = read_file('input/22.in')
    print(len(nodes))
    print('Part 1:', find_viable_pairs(nodes))

if __name__ == "__main__":
    main()
