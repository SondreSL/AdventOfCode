from itertools import count

def redistribute(banks):
    m = banks.index(max(banks))
    num = banks[m]
    banks[m] = 0
    m += 1
    while num > 0:
        banks[m % len(banks)] += 1
        num -= 1
        m += 1

def count_shifts(banks, repeat=False):
    seen = set()
    while True:
        if tuple(banks) in seen:
            if repeat:
                return find_repeat(banks)
            return len(seen)
        seen.add(tuple(banks))
        redistribute(banks)

def find_repeat(banks):
    target = list(banks)
    print(target)
    for i in count(1):
        redistribute(banks)
        if target == banks:
            print(banks)
            return i

def main():
    banks = [int(i) for i in open('input/06.in').readline().split()]
    print('Part 1:', count_shifts(banks))
    print('Part 2:', count_shifts(banks, repeat=True))

if __name__ == "__main__":
    main()

# 2394 Too high
