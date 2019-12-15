from collections import Counter
import re

def read_file(filename):
    nodes = {}
    with open(filename) as f:
        for line in f:
            weight = int(re.findall(r'\d+', line)[0])
            names = re.findall(r'[a-zA-Z]+', line)
            name = names[0]
            if len(names) > 1:
                nodes[name] = (weight, names[1:])
            else:
                nodes[name] = (weight, [])
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

def find_wrong_weight(n, nodes):
    ws = []
    for sub in nodes[n][1]:
        (t, val) = find_wrong_weight(sub, nodes)
        if t:
            return (t, val)
        ws.append(val)
    if len(set(ws)) <= 1:
        return (False, sum(ws) + nodes[n][0])
    else:
        count = Counter(ws).most_common()
        tar = nodes[n][1][ws.index(count[-1][0])]
        tar_sum = nodes[tar][0]
        diff = count[0][0] - count[1][0]
        return (True, tar_sum + diff)

def find_weight(n, nodes):
    ws = [find_weight(c, nodes) for c in nodes[n][1]]
    return sum(ws) + nodes[n][0]

def main():
    nodes = read_file('data/07.in')
    root = find_root(nodes)
    print('Part 1:', root)
    print('Part 2:', find_wrong_weight(root, nodes)[1])

if __name__ == "__main__":
    main()
