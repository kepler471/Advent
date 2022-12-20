from pathlib import Path
from itertools import chain, islice, cycle


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


def flatten(iterable): return list(chain.from_iterable(iterable))


def lmap(func, *iterables): return list(map(func, *iterables))


def manhattan(a, b): return sum(abs(pi - qi) for pi, qi in zip(a, b))


def batched(iterable, n):
    "Batch data into lists of length n. The last batch may be shorter."
    # batched('ABCDEFG', 3) --> ABC DEF G
    if n < 1:
        raise ValueError('n must be at least one')
    it = iter(iterable)
    while batch := list(islice(it, n)):
        yield batch
