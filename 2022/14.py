import sys

import numpy as np

import utils
import string
from typing import Tuple, List
# from queue import PriorityQueue
from collections import abc, namedtuple

Point = Tuple[int, int]


def distance(p: Point, q: Point) -> float:
    dx, dy = abs(fst(p) - fst(q)), abs(snd(p) - snd(q))
    return dx + dy if dx == 0 or dy == 0 else (dx ** 2 + dy ** 2) ** 0.5


def manhattan(p: Point, q: Point) -> int:
    """Distance along grid lines between two points."""
    return sum(abs(pi - qi) for pi, qi in zip(p, q))


def add(p: Point, q: Point) -> Point:
    """Add two points."""
    return fst(p) + fst(q), snd(p) + snd(q)


def sub(p: Point, q: Point) -> Point:
    """Subtract point q from point p."""
    return fst(p) - fst(q), snd(p) - snd(q)


def unit(a, b):
    step_i, step_j = sub(b, a)
    step_i = step_i // abs(step_i) if step_i else 0
    step_j = step_j // abs(step_j) if step_j else 0
    return step_i, step_j


def swap(a, b): return b, a


def mag(a, b): return abs(max(sub(*swap(a, b)), key=abs))


def fst(pair): return pair[0]


def snd(pair): return pair[1]


def first_true(iterable, default=False, pred=None):
    return next(filter(pred, iterable), default)


directions = ((0, 1), (-1, 1), (1, 1))
walls = []


class Grid(dict):
    """A 2D grid, implemented as a mapping of {(x, y): cell_contents}."""

    def __init__(self, mapping_or_rows, directions=directions):
        """Initialize with either (e.g.) `Grid({(0, 0): 1, (1, 0): 2, ...})`, or
        `Grid([(1, 2, 3), (4, 5, 6)])."""
        self.update(mapping_or_rows if isinstance(mapping_or_rows, abc.Mapping) else
                    {(x, y): val
                     for y, row in enumerate(mapping_or_rows)
                     for x, val in enumerate(row)})
        self.width = max(map(fst, self)) + 1
        self.height = max(map(snd, self)) + 1
        self.directions = directions
        self.rocks = rocks

    def find(self, value):
        return first_true(self, pred=lambda x: self[x] == value)

    def is_rock(self, point):
        return True if point in rocks else False

    def neighbors(self, point) -> List[Point]:
        """Points on the grid that neighbor `point`."""
        return [add(point, Δ) for Δ in self.directions if add(point, Δ) in self]

    def to_graph(self, function):
        return {p: [n for n in g.neighbors(p) if g.altitude(n) <= g.altitude(p) + 1]
                for p in g}


import heapq


class PriorityQueue:
    def __init__(self):
        self.elements = []

    def empty(self) -> bool:
        return not self.elements

    def put(self, item, priority: float):
        heapq.heappush(self.elements, (priority, item))

    def get(self):
        return heapq.heappop(self.elements)[1]


import collections


class Queue:
    def __init__(self):
        self.elements = collections.deque()

    def empty(self) -> bool:
        return not self.elements

    def put(self, x):
        self.elements.append(x)

    def get(self):
        return self.elements.popleft()


def bfs(a, b, graph):
    # search = namedtuple("search", "visited cost")

    # frontier = Queue()
    # frontier.put(a)
    # visited = {a: None}
    # cost = {a: 0}
    #
    # while not frontier.empty():
    #     current = frontier.get()
    #
    #     if len(graph[current]) == 0:
    #         return current
    #
    #     for next in graph[current]:
    #         new_cost = cost[current] + 1
    #
    #         if next not in visited:  # cost:# or new_cost < cost[next]:
    #             cost[next] = new_cost
    #             # priority = new_cost# + manhattan(b, next)
    #             frontier.put(next)  # , priority)
    #             visited[next] = current
    current = a
    while True:
        if len(graph[current]) == 0:
            return current

        possible = [move for move in graph[current] if move not in rocks]
        current = possible[0] # graph[current][0] is the first in neighbours



import itertools
from itertools import islice


def generate_points(a, b):
    m = mag(a, b)
    v = unit(a, b)
    pathlist = [a]
    while m > 0:
        pathlist.append(add(pathlist[-1], v))
        m -= 1
    return pathlist


paths = [list(map(eval, line.split(" -> "))) for line in
         utils.read_input(14, 2022, test=False).splitlines()]

rocks = set(list(itertools.chain.from_iterable([
    list(itertools.chain.from_iterable(
        [generate_points(path[n - 1], path[n]) for n, x in enumerate(path) if n != 0]
    ))
    for path in paths
])))

# list(itertools.accumulate(paths[5], lambda x, y: generate_points(x, y)))

points = [[{(x, y): 1} for x in range(550)] for y in range(200)]
g = Grid(points)
start = (500, 0)
end = (500, 199)

graph = {p: [n for n in g.neighbors(p) if not g.is_rock(n)]
         for p in g if not g.is_rock(p)}

floor = 2 + max(map(snd, rocks))
sand = 0

while True:
    current = start
    while True:
        if len(graph[current]) == 0:  # snd(current) == 199 or
            landed = current
            break

        possible = [move for move in graph[current] if move not in rocks]
        if snd(current) == 199 or len(possible) == 0:
            landed = current
            break
        current = possible[0] # graph[current][0] is the first in neighbours



    if snd(landed) == 199:
        print("abyss has been reached, END SIMULATION")
        break
    rocks.add(landed)
    sand += 1

print(sand)

# def sand(a, b, graph):
#     if a in rocks:
#         return

# draw = [[{(x, y): 1} for x in range(550)] for y in range(200)]
# np.set_printoptions(threshold=sys.maxsize)
# # matrix
# draw = np.ones([200,550],dtype="int")
# for i in rocks:
#     draw[swap(*i)] = 0
#
#
# def print_m(m): return "\n".join(["".join(map(str, row)) for row in m])
# print(print_m(draw))
# from matplotlib import pyplot as plt
# plt.imshow(data, interpolation='nearest')
# plt.show()
# a = bfs(start, end, graph)
# print(a[1][end])

# b = [astar(s, end, graph)[1] for s in scenic_starts]
# # c = list(filter(lambda x: end in x, b))
# # print(min(map(lambda x: x[end], c)))
#
# # d = [astar(end, s, graph)[1] for s in scenic_starts]
# # e = list(filter(lambda x: end in x, b))
#
# import time
#
#
# def answer(code: callable):
#     """Verify that calling `code` computes the `correct` answer for `puzzle`.
#     Record results in the dict `answers`. Prints execution time."""
#
#     start = time.time()
#     got = code()
#     dt = time.time() - start
#     print(dt)
#
#
# answer(lambda: astar(start, end, graph))
