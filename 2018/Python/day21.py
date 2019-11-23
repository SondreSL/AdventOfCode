from collections import deque
import numpy as np
from numba import jit

# FUNCS = np.array([lambda xs, x, y: xs[x] + xs[y],
#                   lambda xs, x, y: xs[x] + y,
#                   lambda xs, x, y: xs[x] * xs[y],
#                   lambda xs, x, y: xs[x] * y,
#                   lambda xs, x, y: xs[x] & xs[y],
#                   lambda xs, x, y: xs[x] & y,
#                   lambda xs, x, y: xs[x] | xs[y],
#                   lambda xs, x, y: xs[x] | y,
#                   lambda xs, x, y: xs[x],
#                   lambda xs, x, y: x,
#                   lambda xs, x, y: 1 if x > xs[y] else 0,
#                   lambda xs, x, y: 1 if xs[x] > y else 0,
#                   lambda xs, x, y: 1 if xs[x] > xs[y] else 0,
#                   lambda xs, x, y: 1 if x == xs[y] else 0,
#                   lambda xs, x, y: 1 if xs[x] == y else 0,
#                   lambda xs, x, y: 1 if xs[x] == xs[y] else 0
#                   ])

@jit
def parse(i, v1, v2, res, regs):
    if i == 0:
        regs[res] = regs[v1] + regs[v2]
    elif i == 1:
        regs[res] = regs[v1] + v2
    elif i == 2:
        regs[res] = regs[v1] * regs[v2]
    elif i == 3:
        regs[res] = regs[v1] * v2
    elif i == 4:
        regs[res] = regs[v1] & regs[v2]
    elif i == 5:
        regs[res] = regs[v1] & v2
    elif i == 6:
        regs[res] = regs[v1] | regs[v2]
    elif i == 7:
        regs[res] = regs[v1] | v2
    elif i == 8:
        regs[res] = regs[v1]
    elif i == 9:
        regs[res] = v1
    elif i == 10:
        if v1 > regs[v2]:
            regs[res] = 1
        else:
            regs[res] = 0
    elif i == 11:
        if regs[v1] > v2:
            regs[res] = 1
        else:
            regs[res] = 0
    elif i == 12:
        if regs[v1] > regs[v2]:
            regs[res] = 1
        else:
            regs[res] = 0
    elif i == 13:
        if v1 == regs[v2]:
            regs[res] = 1
        else:
            regs[res] = 0
    elif i == 14:
        if regs[v1] == v2:
            regs[res] = 1
        else:
            regs[res] = 0
    elif i == 15:
        if regs[v1] == regs[v2]:
            regs[res] = 1
        else:
            regs[res] = 0

NAMES = np.array(['addr',
                  'addi',
                  'mulr',
                  'muli',
                  'banr',
                  'bani',
                  'borr',
                  'bori',
                  'setr',
                  'seti',
                  'gtir',
                  'gtri',
                  'gtrr',
                  'eqir',
                  'eqri',
                  'eqrr'
                  ])

def read_file(filename):
    with open(filename) as f:
        data = np.zeros((31, 4), dtype=np.int64)
        pointer = int(f.readline().split()[1])
        for i, string in enumerate(f.read().splitlines()):
            code, v1, v2, res = string.split()
            for j, k in enumerate(NAMES):
                if k == code:
                    code = j
            data[i][0] = code
            data[i][1] = int(v1)
            data[i][2] = int(v2)
            data[i][3] = int(res)
    return data, pointer

@jit
def call(regs, data, inst):
    v = regs[inst]
    f = data[v][0]
    v1 = data[v][1]
    v2 = data[v][2]
    res = data[v][3]
    parse(f, v1, v2, res, regs)
    regs[inst] += 1

@jit(nopython=True)
def run_code(data, inst):
    regs = np.zeros(6, dtype=np.int64)
    previous = np.zeros(15000, dtype=np.int64)
    last = 0
    index = 0
    while True:
        if regs[inst] == 28:
            print('inst == 28')
            if index > 10000:
                for i in range(index):
                    if previous[i] == regs[5]:
                        print('Seen1', regs[5])
                        print('Last:', last)
                        return
            previous[index] = regs[5]
            index += 1
            if index % 1000 == 0:
                print(index)
            last = regs[5]
        call(regs, data, inst)

    print('Register 0:', regs[0])
    return regs[0]


def main():
    data, pointer = read_file('input')
    run_code(data, pointer)
    # reverse()

if __name__ == "__main__":
    main()

# regs = [15180749, 20, 65792, 65536, 257, 14339185]

# 15180749
