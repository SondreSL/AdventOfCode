def is_nice(line):
    if len([i for i in line if i in 'aeiou']) < 3:
        return False
    if not any(a == b for a, b in zip(line, line[1:])):
        return False
    if 'ab' in line:
        return False
    if 'cd' in line:
        return False
    if 'pq' in line:
        return False
    if 'xy' in line:
        return False
    return True

def is_nice_2(line):
    for i in [a+b for a, b in zip(line, line[1:])]:
        if line.count(i) >= 2:
            break
    else:
        return False
    for a,b,c in zip(line, line[1:], line[2:]):
        if a == c:
            return True
    return False

count = 0

for line in open('05.in'):
    if is_nice(line):
        count += 1
print('Part 1:', count)

count = 0
for line in open('05.in'):
    if is_nice_2(line):
        count += 1
print('Part 2:', count)
