import re

def find_exit(nums, negative=False):
    nums = list(nums)
    i = 0
    steps = 0
    while i < len(nums):
        steps += 1
        new = i + nums[i]
        nums[i] += -1 if (negative and nums[i] >= 3) else 1
        i = new
    return steps

def main():
    nums = [int(i) for i in re.findall(r'-*\d+', open('input/05.in').read())]
    print('Part 1:', find_exit(nums))
    print('Part 2:', find_exit(nums, negative=True))

if __name__ == "__main__":
    main()
