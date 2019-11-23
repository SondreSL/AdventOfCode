
# Part 1
with open('02.in') as f:
    total = 0
    for line in f:
        x, y, z = map(int, line.split('x'))
        a = 2 * x * y
        b = 2 * x * z
        c = 2 * y * z
        total += a + b + c + min([x*y, x*z, y*z])
    print('Part 1:', total)

# Part 2
with open('02.in') as f:
    total = 0
    for line in f:
        x, y, z = map(int, line.split('x'))
        a = [x, y, z]
        a.remove(max([x, y, z]))
        total += sum(a) * 2
        total += x * y * z
    print('Part 2:', total)
