
def read_file(filename):
    with open(filename) as f:
        return f.read().splitlines()

REGS = {'a': 1,
        'b': 2,
        'c': 3,
        'd': 4}

def eval_line(regs, ip, instructions):
    """Register 0 is instruction reg."""
    line = instructions[ip]
    print(regs, line)
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
    elif line[0] == 'jnz':
        x = int(regs[REGS[line[1]]]) if line[1] in 'abcd' else int(line[1])
        y = int(regs[REGS[line[2]]]) if line[2] in 'abcd' else int(line[2])
        if x != 0:
            regs[0] += y
            return
    elif line[0] == 'tgl':
        i = regs[REGS[line[1]]] if line[1] in 'abcd' else int(line[1])
        print('Shift by:', i + ip)
        toggle(ip + i, instructions)
    else:
        print('Illegal instruction')
        exit(1)
    regs[0] += 1

# 36897

def toggle(i, instructions):
    if i >= len(instructions):
        return
    print('Inside toggle:', i)
    line = instructions[i].split()
    print('To change:', line)
    if len(line) == 2:
        if line[0] == 'inc':
            instructions[i] = ' '.join(['dec', line[1]])
        else:
            instructions[i] = ' '.join(['inc', line[1]])
    else:
        if line[0] == 'jnz':
            instructions[i] = ' '.join(['cpy', line[1], line[2]])
        else:
            instructions[i] = ' '.join(['jnz', line[1], line[2]])
    print('Changed:', instructions[i])

def run_program(regs, lines):
    ip = regs[0]
    while 0 <= ip < len(lines):
        eval_line(regs, ip, lines)
        # print(regs)
        ip = regs[0]

def main():
    lines = read_file('input/23.in')
    regs = [0, 12, 0, 0, 0]
    run_program(regs, lines)
    print('Part 1:', regs[1])

if __name__ == "__main__":
    main()

