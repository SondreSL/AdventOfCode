from collections import namedtuple

def read_file(filename):
    return open(filename).read().splitlines()

class Point(namedtuple('Point', ['x', 'y'])):
    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

PAD = [[1,2,3],
       [4,5,6],
       [7,8,9]]

PAD2 = [[0,0,1,0,0],
        [0,2,3,4,0],
        [5,6,7,8,9],
        [0,'A','B','C',0],
        [0,0,'D',0,0]]

MOVE = {'L': Point(-1, 0),
        'R': Point(1, 0),
        'U': Point(0, -1),
        'D': Point(0, 1)}

def find_pos(lines, pad, start):
    ans = ''
    pos = Point(*start)
    for line in lines:
        for c in line:
            new = pos + MOVE[c]
            if (0 <= new.y < len(pad)) and (0 <= new.x < len(pad[new.y])) and (pad[new.y][new.x] != 0):
                pos = new
        ans += str(pad[pos.y][pos.x])
    return ans

def main():
    lines = read_file('input/02.in')
    print('Part 1:', find_pos(lines, PAD, (1, 1)))
    print('Part 2:', find_pos(lines, PAD2, (2, 2)))

if __name__ == "__main__":
    main()
