
def read_file(filename):
    with open(filename) as f:
        return f.read().splitlines()

REGS = {'a': 1,
        'b': 2,
        'c': 3,
        'd': 4}

def eval_line(regs, line):
    """Register 0 is instruction reg."""
    line = line.split()
    if line[0] == 'cpy':
        res = REGS[line[2]]
        if line[1] in 'abcd':
            r = REGS[line[1]]
            regs[res] = regs[r]
        else:
            regs[res] = int(line[1])
    elif line[0] == 'inc':
        regs[REGS[line[1]]] += 1
    elif line[0] == 'dec':
        regs[REGS[line[1]]] -= 1
    else:
        if line[1] in 'abcd':
            if regs[REGS[line[1]]] != 0:
                regs[0] += int(line[2])
                return
        else:
            if int(line[1]) != 0:
                regs[0] += int(line[2])
                return
    regs[0] += 1

def run_program(regs, lines):
    ip = regs[0]
    while 0 <= ip < len(lines):
        eval_line(regs, lines[ip])
        # print(regs)
        ip = regs[0]

def main():
    lines = read_file('input/12.in')
    regs = [0, 0, 0, 0, 0]
    run_program(regs, lines)
    print('Part 1:', regs[1])
    regs = [0, 0, 0, 1, 0]
    run_program(regs, lines)
    print('Part 2:', regs[1])

if __name__ == "__main__":
    main()

# 9227737 (Too high)
