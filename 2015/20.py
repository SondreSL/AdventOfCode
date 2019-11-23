import numpy as np

goal = 34_000_000

NUM = 1_000_000

houses_a = np.zeros(NUM)
houses_b = np.zeros(NUM)

for i in range(1, NUM):
    houses_a[i::i] += 10 * i
    houses_b[i:(i + 1) * 50:i] += 11 * i

print('Part 1:', np.nonzero(houses_a >= goal)[0][0])
print('Part 2:', np.nonzero(houses_b >= goal)[0][0])
