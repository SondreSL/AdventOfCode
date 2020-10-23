from itertools import count
import hashlib

key = 'ckczppom'

for i in count():
    if hashlib.md5((key + str(i)).encode()).hexdigest()[:6] == '000000':
        print(f'Part 1: {i}')
        break
