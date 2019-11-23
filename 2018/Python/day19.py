from collections import deque

FUNCS = {'addr': lambda xs, x, y: xs[x] + xs[y],
         'addi': lambda xs, x, y: xs[x] + y,
         'mulr': lambda xs, x, y: xs[x] * xs[y],
         'muli': lambda xs, x, y: xs[x] * y,
         'banr': lambda xs, x, y: xs[x] & xs[y],
         'bani': lambda xs, x, y: xs[x] & y,
         'borr': lambda xs, x, y: xs[x] | xs[y],
         'bori': lambda xs, x, y: xs[x] | y,
         'setr': lambda xs, x, y: xs[x],
         'seti': lambda xs, x, y: x,
         'gtir': lambda xs, x, y: 1 if x > xs[y] else 0,
         'gtri': lambda xs, x, y: 1 if xs[x] > y else 0,
         'gtrr': lambda xs, x, y: 1 if xs[x] > xs[y] else 0,
         'eqir': lambda xs, x, y: 1 if x == xs[y] else 0,
         'eqri': lambda xs, x, y: 1 if xs[x] == y else 0,
         'eqrr': lambda xs, x, y: 1 if xs[x] == xs[y] else 0}

def read_file(filename):
    with open(filename) as f:
        pointer = int(f.readline().split()[1])
        lines = f.read().splitlines()
    return {i: line for i, line in enumerate(lines)}, pointer

def call(regs, data, inst, debug=False):
    f, r1, r2, res = data[regs[inst]].split()
    regs[int(res)] = FUNCS[f](regs, int(r1), int(r2))
    # print(f'{regs} after {f}')
    regs[inst] += 1

def run_code(data, inst, debug=False):
    regs = [1, 0, 0, 0, 0 ,0]
    previous = set()
    count = 0
    while regs[inst] in data:
        call(regs, data, inst, debug=debug)
    print(f'Register 0: {regs[0]}')


def main(debug=False):
    data, pointer = read_file('day19.txt')
    run_code(data, pointer, debug=debug)

if __name__ == "__main__":
    main()


# Part 2 solved manually
