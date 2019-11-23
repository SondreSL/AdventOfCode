import re
from pprint import pprint
import json

def solveA(filename):
    lines = open(filename).read().strip()
    print('Part 1:', sum(int(i) for i in re.findall(r'-*\d+', lines)))

def solveB(filename):
    lines = open(filename).read().strip()
    data = json.loads(lines)
    total = parse(data)
    print('Part 2:', total)

def parse(data):
    total = 0
    if isinstance(data, dict):
        data = list(data.values())
        if 'red' in data:
            return 0
    if isinstance(data, list):
        for i in data:
            total += parse(i)

    try:
        total += data
    except (ValueError,TypeError):
        pass

    return total

def main():
    solveA('12.in')
    solveB('12.in')


if __name__ == "__main__":
    main()


# Attempts: 38885 (Too low)
