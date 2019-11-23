from functools import lru_cache

def read_file(filename):
    lines = {}
    with open(filename) as f:
        for line in f:
            cmd, to = line.split(' -> ')
            lines[to.strip()] = cmd
    return lines

data = read_file('07.in')

@lru_cache()
def calc(val):
    try:
        return int(val)
    except ValueError:
        pass

    cmd = data[val].split(' ')

    if 'NOT' in cmd:
        return ~calc(cmd[1])
    elif 'AND' in cmd:
        return calc(cmd[0]) & calc(cmd[2])
    elif 'OR' in cmd:
        return calc(cmd[0]) | calc(cmd[2])
    elif 'RSHIFT' in cmd:
        return calc(cmd[0]) >> calc(cmd[2])
    elif 'LSHIFT' in cmd:
        return calc(cmd[0]) << calc(cmd[2])
    else:
        return calc(cmd[0])

def main():
    a = calc('a')
    print('Part 1:', a)
    data['b'] = str(a)
    calc.cache_clear()
    a = calc('a')
    print('Part 2:', a)

if __name__ == "__main__":
    main()
