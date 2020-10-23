
def solveA(line):
    return line.count('(') - line.count(')')

def solveB(line):
    count = 0
    pos = 0
    while count != -1:
        pos += 1
        c, line = line[0], line[1:]
        if c == '(':
            count += 1
        if c == ')':
            count -= 1
    return pos

def main():
    line = open('01.in').read()
    print(f'Part 1: {solveA(line)}')
    print(f'Part 2: {solveB(line)}')

if __name__ == "__main__":
    main()
