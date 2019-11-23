
def new_recipe(scoreboard, p1, p2):
    num = scoreboard[p1] + scoreboard[p2]
    temp = []
    while num >= 10:
        temp.append(num % 10)
        num //= 10
    temp.append(num)
    for n in reversed(temp):
        scoreboard.append(n)
    p1 = (p1 + scoreboard[p1] + 1) % len(scoreboard)
    p2 = (p2 + scoreboard[p2] + 1) % len(scoreboard)
    return p1, p2


def solveA(scoreboard, p1, p2, puzzle_input):
    while len(scoreboard) < puzzle_input + 10:
        p1, p2 = new_recipe(scoreboard, p1, p2)
    print(''.join(str(i) for i in scoreboard[puzzle_input:puzzle_input + 10]))


def solveB(scoreboard, p1, p2, puzzle_input):
    """This is a slow solution"""
    puzzle_input = [int(i) for i in str(puzzle_input)]
    # while True:
    while True:
        p1, p2 = new_recipe(scoreboard, p1, p2)
        if puzzle_input[-1] == scoreboard[-1] or puzzle_input[-1] == scoreboard[-2]:
            if scoreboard[-len(puzzle_input):] == puzzle_input:
                print("Found solution:", len(scoreboard) - len(puzzle_input))
                return
            elif scoreboard[-len(puzzle_input) - 1:-1] == puzzle_input:
                print("Found solution:", len(scoreboard) - len(puzzle_input) - 1)
                return



if __name__ == "__main__":
    puzzle_input = 409551

    scoreboard = [3, 7]
    p1 = 0
    p2 = 1
    solveA(scoreboard, p1, p2, puzzle_input)

    scoreboard = [3, 7]
    p1 = 0
    p2 = 1
    solveB(scoreboard, p1, p2, puzzle_input)
