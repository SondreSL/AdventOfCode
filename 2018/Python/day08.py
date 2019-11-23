# Advent of Code 2018 - Day 8

# Part 1

with open('day08.txt') as f:
    data = list(int(i) for i in f.read().split())


def find_metadata(nums):
    children = nums[0]
    n_metadata = nums[1]
    total = 0
    while children:
        meta, sub_nums = find_metadata(nums[2:])
        total += meta
        nums = nums[:2] + sub_nums
        children -= 1
    return total + sum(nums[2:2 + n_metadata]), nums[2 + n_metadata:]


print(find_metadata(data))

# Part 2


def find_value(nums):
    children = nums[0]
    n_metadata = nums[1]
    value = 0
    cs = [0]
    if not children:
        return sum(nums[2:2 + n_metadata]), nums[2 + n_metadata:]
    while children:
        val, sub_sums = find_value(nums[2:])
        cs.append(val)
        nums = nums[:2] + sub_sums
        children -= 1
    for v in nums[2:2 + n_metadata]:
        if v >= len(cs):
            continue
        value += cs[v]
    return value, nums[2 + n_metadata:]


print(find_value(data))
