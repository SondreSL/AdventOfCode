import hashlib
from itertools import count

KEY = 'ahsbgdzn'

def md5_hex(key, additional_hashing):
    for i in count():
        val = hashlib.md5((key + str(i)).encode()).hexdigest()
        for _ in range(additional_hashing):
            val = hashlib.md5(val.encode()).hexdigest()
        yield val

def hash_triple(h):
    for x, y, z in zip(h, h[1:], h[2:]):
        if x == y == z:
            return x

def hash_confirm(hex_hash, val):
    return (val * 5) in hex_hash

def generate_pad(key=KEY, n=64, additional_hashing=0):
    candidates = {}
    keys = set()
    for i, line in enumerate(md5_hex(key, additional_hashing)):
        for cand in list(candidates):
            if hash_confirm(line, candidates[cand][1]):
                keys.add(cand)
                if len(keys) == n:
                    return candidates[cand][2]
                candidates.pop(cand)
            else:
                candidates[cand][0] -= 1
                if candidates[cand][0] == 0:
                    candidates.pop(cand)
        x = hash_triple(line)
        if x is not None:
            candidates[line] = [1000, x, i]

def main():
    print('Part 1:', generate_pad())
    print('Part 2:', generate_pad(additional_hashing=2016))

if __name__ == "__main__":
    main()
