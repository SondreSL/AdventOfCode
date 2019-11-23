from collections import Counter

data = Counter()
mins = {}

with open('day04.txt') as f:
    for line in f:
        line = line.split()
        if len(line) == 1:
            id = line[0]
        else:
            if id not in mins:
                mins[id] = [0] * 60
            time, key, _ = line
            time = int(time[3:])
            end_time = int(next(f).split()[0][3:])
            data[id] += end_time - time
            for i in range(time, end_time):
                mins[id][i] += 1


id = data.most_common(1)[0][0]
minute = mins[id].index(max(mins[id]))

print(int(id) * minute)

# Part 2
curr_max = 0

for guard in mins:
    if max(mins[guard]) > curr_max:
        curr_max = max(mins[guard])
        id = guard

print(mins[id].index(max(mins[id])) * int(id))
