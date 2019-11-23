def read_file(filename):
    rv = []
    with open(filename) as f:
        for line in f:
            line = line.replace(',', '')
            line = line.replace('a', '1')
            line = line.replace('b', '2')
            rv.append(line.split())
    return rv

def func(regs, func, v, w=0):
    v = int(v)
    w = int(w)
    if func == 'hlf':
        regs[v] = regs[v] // 2
    elif func == 'tpl':
        regs[v] *= 3
    elif func == 'inc':
        regs[v] += 1
    elif func == 'jmp':
        regs[0] += int(v)
    elif func == 'jie':
        if regs[v] % 2 == 0:
            regs[0] += int(w)
    elif func == 'jio':
        if regs[v] == 1:
            regs[0] += int(w)

def run_program(regs, data):
    counter = 0
    while 0 <= counter < len(data):
        line = data[counter]
        func(regs, line[0], *line[1:])
        if counter == regs[0]:
            regs[0] += 1
        counter = regs[0]

def main():
    data = read_file('23.in')
    regs = [0, 0, 0]
    run_program(regs, data)
    print('Part 1:', regs[2])
    regs = [0, 1, 0]
    run_program(regs, data)
    print('Part 2:', regs[2])

if __name__ == "__main__":
    main()
