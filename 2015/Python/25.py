from itertools import islice
from math import factorial

FIRST = 20151125

def n_code():
    n = FIRST
    while True:
        yield n
        n = code(n)

def code(previous):
    return (previous * 252533) % 33554393

def top_of_column(n):
    return factorial(n + 1) // (factorial(n - 1) * factorial((n + 1) - (n - 1)))

def place(col, row):
    return top_of_column(col) + sum(range(col, col + (row - 1)))

# Part 1: Code at row 2981, col 3075
def main():
    col = 3075
    row = 2981
    n = place(col, row)
    print(n)
    print('Part 1:', next(islice(n_code(), n - 1, n)))

if __name__ == "__main__":
    main()

# 12322222 (Too high)
# 390292 (Too low)
