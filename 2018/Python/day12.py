"""Advent of Code 2018 Day 12"""

GEN_0 = '.##..##..####..#.#.#.###....#...#..#.#.#..#...#....##.#.#.#.#.#..######.##....##.###....##..#.####.#'

RULES = {
    '#####': '.',
    '####.': '.',
    '###.#': '#',
    '###..': '#',
    '##.##': '.',
    '##.#.': '#',
    '##..#': '.',
    '##...': '#',
    '#.###': '.',
    '#.##.': '.',
    '#.#.#': '#',
    '#.#..': '#',
    '#..##': '#',
    '#..#.': '.',
    '#...#': '.',
    '#....': '.',
    '.####': '#',
    '.###.': '.',
    '.##.#': '.',
    '.##..': '#',
    '.#.##': '#',
    '.#.#.': '#',
    '.#..#': '#',
    '.#...': '#',
    '..###': '#',
    '..##.': '.',
    '..#.#': '.',
    '..#..': '.',
    '...##': '#',
    '...#.': '.',
    '....#': '.',
    '.....': '.',
}


def find_next(section):
    return RULES[''.join(section)]

def find_equal(orig, new):
    while new[0] == '.':
        new = new[1:]
    while new[-1] == '.':
        new = new[:-1]
    return orig == new

def find_sum(gen, padding):
    total = 0
    for v, pot in enumerate(gen, -padding):
        if pot == '#':
            total += v
    return total


def next_generation(current_gen):
    new = '..'
    for pot in range(2, len(current_gen) - 2):
        new += find_next(current_gen[pot-2:pot+3])
    new += '..'
    return new


def main():
    new_gen = GEN_0
    orig = GEN_0[1:]

    padding = 250
    for i in range(padding):
        new_gen = '.' + new_gen + '.'
    
    total = find_sum(new_gen, padding)
    for i in range(1, 200):
        new_gen = next_generation(new_gen)
        i += 1

    old = find_sum(new_gen, padding)
    new_gen = next_generation(new_gen)
    # Generation 200:
    n = 200
    total = find_sum(new_gen, padding)
    diff = total - old
    print(total, diff)
    final_total = total + (50_000_000_000 - 200) * diff
    print(final_total)

    # print(sum(v for v, pot in enumerate(new_gen, -padding) if pot == '#'))

if __name__ == "__main__":
    main()
