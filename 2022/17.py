from utils import *


def rightmost(rock): return max(map(fst, rock))
def leftmost(rock): return min(map(fst, rock))
def top(rock): return max(map(snd, rock))
def bottom(rock): return min(map(snd, rock))
def push(rock, d): return lmap(lambda x: add(x, d), rock)

def can_move(previous, current, dir):
    new = push(current, dir)  # temporarily shift down to see if it collides
    for x in new:
        if x in previous:
            return False
    if leftmost(new) < 0 or rightmost(new) > 6:  # check if it will hit wall
        return False
    if bottom(new) == floor:  # any rock that has managed to hit the floor
        return False
    return True


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
rocks = []  # TODO: instead of tracking each rock, track the highest coord in each col
where_rock = set()
col_maxs = (0, 0, 0, 0, 0, 0, 0)
col_maxs_norm = (0, 0, 0, 0, 0, 0, 0)
while len(rocks) < 5000:
    stops_at = top(lflatten(map(snd, rocks)) + [(0, floor)]) + 1  # find the highest y value in spawned rocks
    spawn_at = stops_at + 3

    if not rocks or fst(rocks[-1]):  # if empty rock list, or the last rock has landed
        rock = next(t)  # spawn rock type
        pos = push(types[rock], (2, spawn_at))  # set rock position
        rocks.append((False, pos, rock, col_maxs, col_maxs_norm))  # turn this on to track each step
    else:
        pos = snd(rocks[-1])
        if not can_move(where_rock, pos, dirs["D"]):
            rocks[-1] = (True, pos, rock, col_maxs, col_maxs_norm)  # rock finally settles
            where_rock |= set(pos)  # add to rock set
            continue
        pos = push(pos, dirs["D"])

    wind = next(moves)
    if can_move(where_rock, pos, dirs[wmap[wind]]):
        pos = push(pos, dirs[wmap[wind]])

    # unique cols occupied by rock
    xs = {fst(xy) for xy in pos}
    # the maximum y val across those cols
    xs_max = {x: top(filter(lambda xy: fst(xy) == x, pos)) for x in xs}
    # replace the col max if the current rock has a higher value
    col_maxs = tuple(max(h, xs_max[n]) if n in xs_max else h for n, h in enumerate(col_maxs))
    # normalise on the first column
    delta = col_maxs[0]
    # calculate th normalised col maximums as a difference of col_maxs[i] to col_maxs[0]
    col_maxs_norm = tuple(h - delta for h in col_maxs)
    rocks[-1] = (False, pos, rock, col_maxs, col_maxs_norm)


print(top(flatten(map(snd, rocks[:-1]))))

# Try a sliding window of 50
norms = lmap(lambda x: x[4], rocks)
maxs = [max([n[k] for n in norms]) for k in range(len(norms[0]))]
mins = [min([n[k] for n in norms]) for k in range(len(norms[0]))]