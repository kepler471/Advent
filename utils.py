from pathlib import Path
from itertools import chain, islice, cycle, starmap, accumulate, pairwise
from more_itertools import flatten, batched, first, first_true, consecutive_groups
from collections import deque, namedtuple, Counter
import numpy as np
import operator as op


def read_input(day, year, test=False):
    """
    Read the input data using read(). Does not do any splitting.
    :param day:
    :param year:
    :param test: whether to use test file or actual input file
    :return: raw input data
    """
    filename = "input" if not test else "test"
    path = Path().cwd() / (filename + str(day) + ".txt")
    with open(path) as file:
        data = file.read()
    return data


def add(a, b): return fst(a) + fst(b), snd(a) + snd(b)


def sub(a, b): return fst(a) - fst(b), snd(a) - snd(b)


def unit(a, b):
    step_i, step_j = sub(b, a)
    step_i = step_i // abs(step_i) if step_i else 0
    step_j = step_j // abs(step_j) if step_j else 0
    return step_i, step_j


def swap(a, b): return b, a


def mag(a, b): return abs(max(sub(*swap(a, b)), key=abs))


def fst(pair): return pair[0]


def snd(pair): return pair[1]


def lmap(func, *iterables): return list(map(func, *iterables))


def manhattan(a, b): return sum(abs(pi - qi) for pi, qi in zip(a, b))


def lflatten(iterable): return list(flatten(iterable))


def connected(a, b): return any(map(lambda x, y: abs(y - x) == 1, zip(a, b)))


def bfs(a, b, graph):
    search = namedtuple("search", ["visited", "cost"])

    frontier = deque()
    frontier.append(a)
    visited = {a: None}
    cost = {a: 0}

    while not frontier:
        current = frontier.popleft()

        if current == b:
            break

        for next in graph[current]:
            new_cost = cost[current] + 1

            if next not in visited:  # cost:# or new_cost < cost[next]:
                cost[next] = new_cost
                # priority = new_cost# + manhattan(b, next)
                frontier.append(next)  # , priority)
                visited[next] = current
    return search(visited, cost)


def find_runs(x):
    """Find runs of consecutive items in an array."""

    # ensure array
    x = np.asanyarray(x)
    if x.ndim != 1:
        raise ValueError('only 1D array supported')
    n = x.shape[0]

    # handle empty array
    if n == 0:
        return np.array([]), np.array([]), np.array([])

    else:
        # find run starts
        loc_run_start = np.empty(n, dtype=bool)
        loc_run_start[0] = True
        np.not_equal(x[:-1], x[1:], out=loc_run_start[1:])
        run_starts = np.nonzero(loc_run_start)[0]

        # find run values
        run_values = x[loc_run_start]

        # find run lengths
        run_lengths = np.diff(np.append(run_starts, n))

        return run_values, run_starts, run_lengths


def clamp(x, lo, hi): return max(lo, min(x, hi))
