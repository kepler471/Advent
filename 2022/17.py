from utils import *


def rightmost(rock): return max(map(fst, rock))
def leftmost(rock): return min(map(fst, rock))
def top(rock): return max(map(snd, rock))
def bottom(rock): return min(map(snd, rock))
def push(rock, d): return lmap(lambda x: add(x, d), rock)

def can_fall(previous, current):
    new = push(current, dirs["D"])
    if len(previous) > 1:
        prev = previous[:-1]
        for x in new:
            if x in flatten(map(snd, prev)):
                return False
    if bottom(new) == floor:
        return False
    return True

def at_wall(rock, d):
    if d == "L":
        return True if leftmost(rock) == 0 else False
    elif d == "R":
        return True if rightmost(rock) == 6 else False


types = {
    "_": [(0, 0), (1, 0), (2, 0), (3, 0)],
    "+": [(0, 1), (1, 1), (2, 1), (1, 0), (1, 2)],
    "⅃": [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
    "|": [(0, 0), (0, 1), (0, 2), (0, 3)],
    "◻": [(0, 0), (1, 0), (0, 1), (1, 1)],
}
dirs = {"U": (0, 1), "D": (0, -1), "L": (-1, 0), "R": (1, 0)}
wmap = {"<": "L", ">": "R"}

moves = cycle(list(read_input(17, 2022, test=True)))
t = cycle(types)
floor = 0
rocks = []

while len(rocks) < 2023:
# for n in range(30):
    print(f"n = , len(rocks) = {len(rocks)}")
    stops_at = top(flatten(map(snd, rocks)) + [(0, floor)]) + 1  # find the highest y value in spawned rocks
    spawn_at = stops_at + 3

    if not rocks or fst(rocks[-1]):  # if empty rock list, or the last rock has landed
        rock = next(t)  # spawn rock type
        pos = push(types[rock], (2, spawn_at))  # set rock position
        print(f"1. rock {rock} is spawned as {pos}")
        rocks.append((False, pos, rock))
    else:
        pos = snd(rocks[-1])
        print(f"1. rock at {pos} is selected")
        if not can_fall(rocks, pos):
            print(f"2. rock can't fall, go next ####################")
            rocks[-1] = (True, pos, rock)
            continue
        pos = push(pos, dirs["D"])
        print(f"2. rock falls to {pos}")

    wind = next(moves)
    if not at_wall(pos, wmap[wind]):
        print(f"3. wind {wind} pushes to  {push(pos, dirs[wmap[wind]])}")
        pos = push(pos, dirs[wmap[wind]])
    else:
        print(f"3. wind {wind} cant push {wmap[wind]}, rock at wall, {pos}")

    rocks[-1] = (False, pos, rock)


print(top(flatten(map(snd, rocks[:-1]))))
