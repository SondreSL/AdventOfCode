def valid_passphrases(lines):
    return sum(1 for line in lines if (len(line) == len(set(line))))

def anagrams(lines):
    lines = [[''.join(sorted(l)) for l in line] for line in lines]
    return sum(1 for line in lines if len(line) == len(set(line)))

def main():
    lines = [line.split() for line in open('input/04.in').read().splitlines()]
    print('Part 1:', valid_passphrases(lines))
    print('Part 2:', anagrams(lines))

if __name__ == "__main__":
    main()
