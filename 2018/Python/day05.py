from string import ascii_lowercase
from string import ascii_uppercase

with open('day05.txt') as f:
    line = f.read().strip()


def reaction(string):
    new = ""
    string += " "
    it = zip(string, string[1:])
    for a, b in it:
        if a.lower() != b.lower():
            new += a
            continue
        if a == b:
            new += a
            continue
        try:
            next(it)
        except:
            pass
    string = string[:-1]
    if string == new:
        return False
    else:
        return new

# new = line
# while new:
#     line = new
#     new = reaction(line)

# print(len(line))

# Part 2
letters = zip(ascii_uppercase, ascii_lowercase)
orig = line
for a, b in letters:
    line = orig
    while a in line:
        i = line.index(a)
        line = line[:i] + line[i + 1:]
    while b in line:
        i = line.index(b)
        line = line[:i] + line[i + 1:]
    new = line
    while new:
        line = new
        new = reaction(line)
    print(len(line))

