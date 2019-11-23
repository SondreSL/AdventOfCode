from itertools import combinations
from functools import reduce
from operator import mul

DATA = [int(i) for i in open('24.in').read().splitlines()]

def find_sublists(seq, n):
    weight = sum(seq) // n
    for i in range(2, len(seq)):
        for c in combinations(reversed(seq), i):
            if sum(c) == weight:
                subseq = set(seq) - set(c)
                while subseq:
                    for j in range(2, len(subseq)):
                        for cc in combinations(subseq, j):
                            if sum(cc) == weight:
                                subseq -= set(cc)
                                break
                        else:
                            continue
                        break
                    else:
                        subseq = set()
                        continue
                yield c


def sublist(data, n):
    seen = set()
    for i in range(4, 5):
        for s in combinations(data, i):
            if sum(s) == n:
                second = set(data) - set(s)
                for j in range(2, len(second)):
                    for r in combinations(second, j):
                        if sum(r) == n:
                            third = second - set(r)
                            for k in range(2, len(third)):
                                for t in combinations(third, k):
                                    if sum(t) == n:
                                        fourth = third - set(t)
                                        if sum(fourth) == n:
                                            break
                                else:
                                    continue
                                break
                            else:
                                continue
                    else:
                        continue
                    break
                else:
                    continue
                if sorted(s) in seen:
                    continue
                else:
                    seen.add(sorted(s))
                    yield s


def find_best(data, n):
    a = find_sublists(data, n)
    first = next(a)
    l = len(first)
    qe = reduce(mul, first)
    while len(first) == l:
        first = next(a)
        if reduce(mul, first) < qe:
            qe = reduce(mul, first)
    return qe


print('Part 1:', find_best(DATA, 3))
print('Part 2:', find_best(DATA, 4))
# print('Test:', find_best(DATA, func=test))

# Attempts:  163845007999 (Too high)
#            11846773891

