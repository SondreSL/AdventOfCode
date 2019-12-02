import re

def read_file(filename):
    with open(filename) as f:
        return list(map(int, re.findall("\d+", f.read())))

def compute(data):
    index = 0
    while data[index] != 99:
        if data[index] == 1:
            data[data[index + 3]] = data[data[index + 1]] + data[data[index + 2]]
        elif data[index] == 2:
            data[data[index + 3]] = data[data[index + 1]] * data[data[index + 2]]
        else:
            print("Bad opcode")
            exit(1)
        index += 4
    return data[0]

def solveA(data):
    data[1] = 12
    data[2] = 2
    return compute(data)

def solveB(data):
    for i in range(0, 100):
        for j in range(0, 100):
            new = data[:]
            new[1] = i
            new[2] = j
            if compute(new) == 19690720:
                return 100 * i + j

def main():
    data = read_file("../data/input-2019-2.txt")
    print('Part A: ', solveA(data[:]))
    print('Part B: ', solveB(data))

if __name__ == "__main__":
    main()

# Part A: 3306701
# Part B: 7621
