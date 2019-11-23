
def read_file(filename):
    with open(filename) as f:
        return [[int(i) for i in line.split()] for line in f]

def checksum(lines):
    return sum(max(i) - min(i) for i in lines)

def evenly_divisible(lines):
    total = 0
    for line in lines:
        for a in line:
             for b in line:
                 if a == b:
                     continue
                 if a % b == 0:
                     total += (a // b)
    return total

def main():
    lines = read_file('input/02.in')
    print('Part 1:', checksum(lines))
    print('Part 2:', evenly_divisible(lines))

if __name__ == "__main__":
    main()

# 541 (Too high)
