import re
from operator import mul
from operator import add

def read_file(filename):
    with open(filename) as f:
        return list(map(int, re.findall("\d+", f.read())))

def compute(data):
    index = 0
    while data[index] != 99:
        data[data[index + 3]] = {1: add, 2: mul}[data[index]](data[data[index + 1]], data[data[index + 2]])
        index += 4
    return data[0]

def solveA(data):
    return compute(data[:1] + [12, 2] + data[3:])

def solveB(data):
    for i in range(0, 100):
        for j in range(0, 100):
            if compute(data[:1] + [i, j] + data[3:]) == 19690720:
                return 100 * i + j

def main():
    data = read_file("data/input-2019-2.txt")
    print('Part A: ', solveA(data[:]))
    print('Part B: ', solveB(data))

if __name__ == "__main__":
    main()

# Part A: 3306701
# Part B: 7621
