
INIT = '01110110101001000'

def gen(a):
    b = ''.join('1' if i == '0' else '0' for i in reversed(a))
    return a + '0' + b

assert gen('111100001010') == '1111000010100101011110000'

def checksum(string):
    new = ''
    for a, b in list(zip(string, string[1:]))[::2]:
        new += '1' if a == b else '0'
    return new

def gen_checksum(data, length):
    cs = ''
    while len(data) < length:
        data = gen(data)
    cs = checksum(data[:length])
    while len(cs) % 2 == 0:
        if len(cs) > length:
            cs = cs[:length]
        cs = checksum(cs)
    return cs

def main():
    print('Part 1:', gen_checksum(INIT, 272))
    print('Part 1:', gen_checksum(INIT, 35651584))

if __name__ == "__main__":
    main()

