from collections import defaultdict
from pprint import pprint
from string import ascii_uppercase
from bisect import insort

# Part 1

dependsOn = defaultdict(list)

with open('day07.txt') as f:
    for line in f:
        a, b = line.split()
        dependsOn[b].append(a)

a = [let for let in ascii_uppercase if let not in dependsOn]

result = ""

while a:
    val = a.pop(0)
    for k, v in dependsOn.items():
        if val in v:
            dependsOn[k].remove(val)
        if len(dependsOn[k]) == 0:
            insort(a, k)
    for e in a:
        if e in dependsOn:
            dependsOn.pop(e)
    result += val

print(result)

# Part 2

dependsOn = defaultdict(list)
WORKERS = 5
active_workers = 0
time = 0
end_time = dict(zip(ascii_uppercase, range(61, 100)))
waiting = []

with open('day07.txt') as f:
    for line in f:
        a, b = line.split()
        dependsOn[b].append(a)

active = [let for let in ascii_uppercase if let not in dependsOn]
active_workers = len(active)
active.sort(key=lambda x: end_time[x])

while active:
    active.sort(key=lambda x: end_time[x])
    val = active.pop(0)
    active_workers -= 1
    time = end_time[val]
    for k, v in dependsOn.items():
        if val in v:
            dependsOn[k].remove(val)
        if len(dependsOn[k]) == 0:
            insort(waiting, k)
    for w in waiting:
        if w in dependsOn:
            dependsOn.pop(w)
    while active_workers < 5 and waiting:
        new = waiting.pop(0)
        end_time[new] += time
        active.append(new)

print(time)
