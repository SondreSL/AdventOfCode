
KEY = 289326

def find_steps(key):
    if key == 1:
        return 0
    for ring, n in enumerate(range(1, 30000, 2)):
        if n ** 2 >= key:
            break
    a = n ** 2 - (n // 2)
    axes = [a - (i * (n - 1)) for i in range(4)]
    return min(abs(key - i) for i in axes) + ring

assert find_steps(12) == 3
assert find_steps(23) == 2
assert find_steps(1024) == 31

def main():
    print('Part 1:', find_steps(KEY))

if __name__ == "__main__":
    main()
