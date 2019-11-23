from itertools import permutations

def operate(string, line):
    line = line.split()
    if len(line) == 4 and line[0] == 'rotate':
        steps = int(line[2])
        if line[1] == 'right':
            string = string[-steps:] + string[:-steps]
        else:
            string = string[steps:] + string[:steps]
    elif line[0] == 'rotate':
        let = string.index(line[6]) + 1
        let = let + 1 if let > 4 else let
        cmd = f'rotate right {let} steps'
        string = operate(string, cmd)
    elif line[0] == 'swap':
        if line[1] == 'position':
            a, b = int(line[2]), int(line[5])
            temp = string[a]
            string = string[:a] + string[b] + string[a + 1:]
            string = string[:b] + temp + string[b + 1:]
        else:
            string = string.replace(line[2], '0')
            string = string.replace(line[5], line[2])
            string = string.replace('0', line[5])
    elif line[0] == 'reverse':
        a, b = int(line[2]), int(line[4])
        rev = string[a:b + 1]
        rev = rev[::-1]
        string = string[:a] + rev + string[b + 1:]
    elif line[0] == 'move':
        a, b = int(line[2]), int(line[5])
        let = string[a]
        string = string[:a] + string[a + 1:]
        string = string[:b] + let + string[b:]
    return string

assert operate('abcdefgh', 'rotate right 4 steps') == 'efghabcd'
assert operate('abcdefgh', 'rotate left 3 steps') == 'defghabc'
assert operate('abcdefgh', 'swap position 2 with position 4') == 'abedcfgh'
assert operate('abcdefgh', 'swap letter c with position f') == 'abfdecgh'
assert operate('abcdefgh', 'rotate based on position of letter d') == 'efghabcd'
assert operate('abcdefgh', 'rotate based on position of letter f') == 'bcdefgha'
assert operate('abcdefgh', 'move position 2 to position 6') == 'abdefgch'
assert operate('abcdefgh', 'reverse positions 3 through 5') == 'abcfedgh'

def scramble(string, lines):
    for line in lines:
        string = operate(string, line)
    return string

def unscramble(target, lines):
    for perm in permutations('abcdefgh'):
        cand = ''.join(perm)
        try:
            if scramble(cand, lines) == target:
                return cand
        except ValueError:
            continue

def main():
    lines = open('input/21.in').read().splitlines()
    print('Part 1:', scramble('abcdefgh', lines))
    print('Part 2:', unscramble('fbgdceah', lines))

if __name__ == "__main__":
    main()
