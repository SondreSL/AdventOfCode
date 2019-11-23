from itertools import count
import hashlib


def md5_hex(key):
    for i in count():
        val = hashlib.md5((key + str(i)).encode()).hexdigest()
        if val[:5] == '0' * 5:
            yield val

def solveA(key, length=8, zeros=5):
    password = ''
    for val in md5_hex(key):
        password += val[zeros]
        print('Processing ... :', ''.join(password))
        if len(password) == length:
            return password

def solveB(key):
    password = [''] * 8
    for val in md5_hex(key):
        try:
            i = int(val[5])
        except ValueError:
            continue
        if 0 > i or i > 7:
            continue
        if password[i] == '':
            password[i] = val[6]
            print('Processing: ... :', ''.join(password))
            if '' not in password:
                return ''.join(password)

def main():
    key = 'wtnhxymk'
    print('Part 1:', solveA(key))
    print('Part 2:', solveB(key))

if __name__ == "__main__":
    main()
