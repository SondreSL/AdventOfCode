
def safe_tiles(line, rows=10):
    count = line.count('.')
    for _ in range(rows - 1):
        line = '.' + line + '.'
        new = ''
        for a, b, c in zip(line, line[1:], line[2:]):
            if '^' == a == b != c:
                new += '^'
            elif '^' == b == c != a:
                new += '^'
            elif '^' == a != b == c:
                new += '^'
            elif a == b != c == '^':
                new += '^'
            else:
                new += '.'
        line = new
        count += line.count('.')
    return count

assert safe_tiles('.^^.^.^^^^', rows=10) == 38

def main():
    line = '.^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^.'
    print('Part 1:', safe_tiles(line, rows=40))
    print('Part 2:', safe_tiles(line, rows=400000))

if __name__ == "__main__":
    main()
