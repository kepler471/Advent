# --- Day 12: Hill Climbing Algorithm ---
#
# You try contacting the Elves using your handheld device, but the river you're following must be too low to get a
# decent signal.
#
# You ask the device for a heightmap of the surrounding area (your puzzle input). The heightmap shows the local area
# from above broken into a grid; the elevation of each square of the grid is given by a single lowercase letter,
# where a is the lowest elevation, b is the next-lowest, and so on up to the highest elevation, z.
#
# Also included on the heightmap are marks for your current position (S) and the location that should get the best
# signal (E). Your current position (S) has elevation a, and the location that should get the best signal (E) has
# elevation z.
#
# You'd like to reach E, but to save energy, you should do it in as few steps as possible. During each step,
# you can move exactly one square up, down, left, or right. To avoid needing to get out your climbing gear,
# the elevation of the destination square can be at most one higher than the elevation of your current square; that
# is, if your current elevation is m, you could step to elevation n, but not to elevation o. (This also means that
# the elevation of the destination square can be much lower than the elevation of your current square.)
#
# For example:
#
# Sabqponm
# abcryxxl
# accszExk
# acctuvwj
# abdefghi
#
# Here, you start in the top-left corner; your goal is near the middle. You could start by moving down or right,
# but eventually you'll need to head toward the e at the bottom. From there, you can spiral around to the goal:
#
# v..v<<<<
# >v.vv<<^
# .>vv>E^^
# ..v>>>^^
# ..>>>>>^
#
# In the above diagram, the symbols indicate whether the path exits each square moving up (^), down (v), left (<),
# or right (>). The location that should get the best signal is still E, and . marks unvisited squares.
#
# This path reaches the goal in 31 steps, the fewest possible.
#
# What is the fewest steps required to move from your current position to the location that should get the best signal?


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


def fst(pair): return pair[0]


def snd(pair): return pair[1]


def first_true(iterable, default=False, pred=None):
    return next(filter(pred, iterable), default)


directions = ((0, -1), (0, 1), (1, 0), (-1, 0))


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

    def find(self, value):
        return first_true(self, pred=lambda x: self[x] == value)

    def altitude(self, point: Point):
        heights = {string.ascii_lowercase[h]: h for h in range(len(string.ascii_lowercase))}
        return heights[self[point]]

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


def astar(a, b, graph):
    search = namedtuple("search", "visited cost")

    frontier = PriorityQueue()
    frontier.put(a, 0)
    visited = {a: None}
    cost = {a: 0}

    while not frontier.empty():
        current = frontier.get()

        if current == b:
            break

        for next in graph[current]:
            new_cost = cost[current] + 1

            if next not in cost or new_cost < cost[next]:
                cost[next] = new_cost
                priority = new_cost + manhattan(b, next)
                frontier.put(next, priority)
                visited[next] = current
    return search(visited, cost)


def djikstra(a, b, graph):
    search = namedtuple("search", "visited cost")

    frontier = PriorityQueue()
    frontier.put(a, 0)
    visited = {a: None}
    cost = {a: 0}

    while not frontier.empty():
        current = frontier.get()

        if current == b:
            break

        for next in graph[current]:
            new_cost = cost[current] + 1

            if next not in cost or new_cost < cost[next]:
                cost[next] = new_cost
                priority = new_cost  # + manhattan(b, next)
                frontier.put(next, priority)  # , priority)
                visited[next] = current
    return search(visited, cost)


def bfs(a, b, graph):
    search = namedtuple("search", "visited cost")

    frontier = Queue()
    frontier.put(a)
    visited = {a: None}
    cost = {a: 0}

    while not frontier.empty():
        current = frontier.get()

        if current == b:
            break

        for next in graph[current]:
            new_cost = cost[current] + 1

            if next not in visited:  # cost:# or new_cost < cost[next]:
                cost[next] = new_cost
                # priority = new_cost# + manhattan(b, next)
                frontier.put(next)  # , priority)
                visited[next] = current
    return search(visited, cost)


data = list(map(list, utils.read_input(12, 2022, test=False).split()))

g = Grid(data)
start = g.find("S")
end = g.find("E")
g[start] = "a"
g[end] = "z"

graph = {p: [n for n in g.neighbors(p) if g.altitude(n) <= g.altitude(p) + 1]
         for p in g}

scenic_starts = [p for p in g if g[p] == "a"]

a = astar(start, end, graph)
# print(a[1][end])
# b = [astar(s, end, graph)[1] for s in scenic_starts]
# c = list(filter(lambda x: end in x, b))
# print(min(map(lambda x: x[end], c)))

# d = [astar(end, s, graph)[1] for s in scenic_starts]
# e = list(filter(lambda x: end in x, b))

import time


def answer(code: callable):
    """Verify that calling `code` computes the `correct` answer for `puzzle`.
    Record results in the dict `answers`. Prints execution time."""

    start = time.time()
    got = code()
    dt = time.time() - start
    print(dt)


x = "BFS"
answer(lambda: astar(start, end, graph))

# TODO: Using the adjacency matrix below, can we run a simulation of path-taking? Take the matrix to be a Markov
#  matrix.

# TODO: Is there an FFT solution to the problem?

# TODO: Is there an eigenvalue solution to the problem?

# TODO: Is there a MapReduce solution? can that be run on Spark? what about PageRank?

# TODO: Could the matrix be chunked into grids, to shrink matrix size? Find consecutive grids?

from itertools import chain, product

## build adjacency list[tuple[a -> b]]
adj = list(chain.from_iterable([
    list(product([p], [n for n in g.neighbors(p) if g.altitude(n) <= g.altitude(p) + 1]))
    for p in g
]))

# build adjacency matrix list[list[bool]]
dim = g.height * g.width
matrix = [[0 for _ in range(dim)] for _ in range(dim)]
for a in adj:
    from_row = a[0][1]
    from_col = a[0][0]
    to_row = a[1][1]
    to_col = a[1][0]
    matrix[g.height * from_col + from_row][g.height * to_col + to_row] = 1


# matrix print
def print_m(m): return "\n".join(["".join(map(str, row)) for row in m])


import sys
import numpy as np
from numpy.linalg import matrix_power


# convert from grid indices to matrix index
def g_to_m(point): return g.height * fst(point) + snd(point)


# convert from matrix index to grid indices
def m_to_g(index): return index // g.height, index % g.height


np.set_printoptions(threshold=100)

m = np.matrix(matrix)  # use numpy now
norm = (m / m.sum(axis=1))  # normalise so that each row sums to one.
nrows = len(norm)

runs = []  # store those runs


def simulate(transitions, n_runs, max_steps):
    # how many simulations to run = n_runs
    for r in range(n_runs):
        norm = (transitions / transitions.sum(axis=1))  # fresh normalised matrix
        # limit the length of each simulation = max_steps
        steps = np.zeros(max_steps, dtype=np.uint64)
        current = 0  # start with (0, 0)
        for i in range(max_steps):
            steps[i] = current  # log visited points

            # Do not want to revisit "current", so eliminate all possible transitions to "current"
            norm_rows = norm[:, current].nonzero()[0]  # find the possible transitions to "current"

            norm[norm_rows, current] = 0  # set visited point to unvisitable
            norm[norm_rows] = norm[norm_rows] / norm[norm_rows].sum(axis=1)  # re-normalise the affected rows
            # TODO: what about weighting the probabilities to make less dumb transitions?
            if current == g_to_m(end):
                runs.append(steps[1: i + 1])
                print(f"found goal during run {r}, with length {runs[-1]}")
                break
            row = norm[current, :]
            indices = row.nonzero()[1].tolist()
            nonzero_probs = [norm[current, x] for x in indices]
            if len(nonzero_probs) < 1 or sum(nonzero_probs) != 1:
                break
            current = np.random.choice(indices, 1, p=nonzero_probs)[0]

        if (r + 1) % 10_000 == 0:
            print(f"at run {r + 1}. currently there are {len([len(r) for r in runs])} solutions.")

    return runs


runs_results = simulate(m, 500_000, 500)

# success = [run for run in runs if 27 in run]
# print(len(runs))
# print(min(map(len, runs)))

# np.bincount(steps) / num_steps
#
# eigenStuff = np.linalg.eig(norm)
# # eigenStuff[1][:,1]/np.sum(eigenStuff[1][:,1])
# np.array(eigenStuff[1][:, 1] / np.sum(eigenStuff[1][:, 1])).flatten()

# TODO: write up sparse matrix solution here. test if setting all nonzeros to one at each multiplication optimises
