from collections import namedtuple
from collections import deque
from random import shuffle

def read_file(filename):
    codes = set()
    with open(filename) as f:
        string = f.readline().strip()
        for line in f:
            f, t = line.strip().split(' => ')
            codes.add((f, t))
    return string, codes

def possible_mutations(molecule, codes):
    seen = set()
    for f, t in codes:
        seen.update(mutations(molecule, f, t))
    return seen

def mutations(molecule, f, t):
    muts = set()
    c = molecule.count(f)
    for i in range(c):
        new = molecule.split(f)
        new[i] = t.join([new[i], new[i + 1]])
        new.pop(i + 1)
        muts.add(f.join(new))
    return muts

def solveA(molecule, codes):
    print('Part 1:', len(possible_mutations(molecule, codes)))

def solveB(start, target, codes):
    codes = set((v, k) for k, v in codes)
    string = ''
    while string != start:
        count = 0
        string = target
        while len(string) > 1:
            new = possible_mutations(string, codes)
            if not new:
                break
            new = list(new)
            shuffle(new)
            string = new[0]
            count += 1
    print('Part 2:', count)

def main():
    molecule, codes = read_file('19.in')
    solveA(molecule, codes)
    solveB('e', molecule, codes)

if __name__ == "__main__":
    main()
