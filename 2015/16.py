from ast import literal_eval
import re

INFO = {'children': 3,
        'cats': 7,
        'samoyeds': 2,
        'pomeranians': 3,
        'akitas': 0,
        'vizslas': 0,
        'goldfish': 5,
        'trees': 3,
        'cars': 2,
        'perfumes': 1}

def read_file(filename):
    data = {}
    with open(filename) as f:
        for line in f:
            n, attrs = re.findall(r'Sue (\d+): (.*$)', line)[0]
            attrs = attrs.split(',')
            data[int(n)] = {i[:-1]: int(j) for i, j in map(str.split, attrs)}
    return data

def solveA(data):
    find_sue(data, INFO)

def solveB(data):
    find_sue(data, INFO, part2=True)

def find_sue(data, INFO, part2=False):
    data = dict(data)
    for sue, attr in list(data.items()):
        for a, val in attr.items():
            if part2 and a in ['cats', 'trees']:
                if INFO[a] >= val:
                    data.pop(sue)
                    break
                continue
            if part2 and a in ['pomeranians', 'goldfish']:
                if INFO[a] <= val:
                    data.pop(sue)
                    break
                continue
            if INFO[a] != val:
                data.pop(sue)
                break
    print('Part 1:', list(data)[0])

def main():
    data = read_file('16.in')
    solveA(data)
    solveB(data)

if __name__ == "__main__":
    main()
