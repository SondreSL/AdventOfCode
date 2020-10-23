from string import ascii_lowercase
import re

def legal(string):
    return req1(string) and req2(string) and req3(string)

def req1(string):
    return any(a+b+c in ascii_lowercase for a, b, c in zip(string, string[1:], string[2:]))

def req2(string):
    return not any(c in string for c in 'iol')

def req3(string):
    return len(re.findall(r'(\w{1})(\1)', string)) >= 2

def iterate_pw(string):
    # Can be a lot more efficient if it skips illegal characters
    string = list(string)
    curr = -1
    while True:
        if string[curr] == 'z':
            string[curr] = 'a'
            curr -= 1
        else:
            string[curr] = ascii_lowercase[ascii_lowercase.index(string[curr]) + 1]
            return ''.join(string)

def main():
    a = 'cqjxjnds'
    while not legal(a):
        a = iterate_pw(a)
    print('Part 1:', a)
    a = iterate_pw(a)
    while not legal(a):
        a = iterate_pw(a)
    print('Part 2:', a)

if __name__ == "__main__":
    main()
