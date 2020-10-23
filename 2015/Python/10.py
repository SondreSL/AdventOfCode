import re

def play(key):
    new = ''
    while key:
        f = key[0]
        l = 1
        key = key[1:]
        while key and key[0] == f:
            l += 1
            key = key[1:]
        new += str(l) + f
    return new

def run(times=40):
    key = '3113322113'
    for _ in range(times):
        key = play(key)
    print(len(key))

def main():
    # It's slow, but it works
    run()
    run(times=50)

if __name__ == "__main__":
    main()
