import functools
import operator
from dataclasses import dataclass


@dataclass
class Vector:
    x: tuple

    dirs = {
        "U": (-1, 0),
        "D": (1, 0),
        "L": (0, -1),
        "R": (0, 1),
    }

    def __getitem__(self, item):
        return self.x[item]

    def __add__(self, other):
        return Vector(tuple(map(operator.add, self, other)))

    def __sub__(self, other):
        return Vector(tuple(map(operator.sub, self, other)))

    def __hash__(self):
        return hash(self.x)

    def manhattan(self: (int, int), b: (int, int)):
        return abs(self[0] - b[0]) + abs(self[1] - b[1])  # how to do this functionally with maps and operators?

    def unit(self, b):
        step_i, step_j = self - b
        step_i = step_i // abs(step_i) if step_i else 0
        step_j = step_j // abs(step_j) if step_j else 0
        return step_i, step_j


def follow(b: Vector, a: Vector):
    if a.manhattan(b) == 2 and (a[0] == b[0] or a[1] == b[1]):
        return b + a.unit(b)
    elif a.manhattan(b) >= 3 and a[0] != b[0] and a[1] != b[1]:
        return b + a.unit(b)
    return b


def simulate_rope(n_knots):
    knots = [Vector((0, 0)) for _ in range(n_knots)]
    tracking = []

    for mov in moves_i:
        knots[0] = knots[0] + Vector.dirs[mov[0]]
        for n in range(1, n_knots):
            knots[n] = follow(knots[n], knots[n - 1])

        tracking.append(knots[-1])

    print(f"For a rope \n{'--'.join(n_knots * ['*'])}\nthe tail visits {len(set(tracking))} points.\n")
    return tracking


with open("input9.txt", "r") as input9:
    lines = input9.read().splitlines()

moves = [(x.split()[0], int(x.split()[1])) for x in lines]
moves_i = functools.reduce(
    operator.iconcat,
    [x[2] * [(x[0], x[1])] for x in [(y[0], y[1] // y[1], y[1]) for y in moves]]
)

simulate_rope(2)
simulate_rope(10)