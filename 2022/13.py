import utils


def fst(items): return items[0]


def snd(items): return items[1]


def tail(items): return items[1:]


def swap(a, b): return b, a


logging=False
def order(a, b):
    print(f"order called for {a, b}") if logging else None

    # match a, b:
    #     case list
    if type(a) == type(b) == list and (len(a) == 0 or len(b) == 0):
        print(f"a and b are lists, and one of them is empty") if logging else None
        if a < b:
            print(f"a and b are lists, and one of them is empty. It is a") if logging else None
            return 1
        if a > b:
            print(f"a and b are lists, and one of them is empty. It is a") if logging else None
            return -1

    if type(fst(a)) == type(fst(b)) == list:
        if len(fst(a)) == 0 and len(fst(b)) != 0:
            print(f"{fst(a)} == 0 and {fst(b)} != 0, return 1") if logging else None
            return 1
        elif len(fst(a)) != 0 and len(fst(b)) == 0:
            print(f"{fst(a)} != 0 and {fst(b)} == 0, return -1") if logging else None
            return -1
        elif len(fst(a)) == 0 and len(fst(b)) == 0:
            print(f"{fst(a)} == 0 and {fst(b)} == 0, recurse on tail") if logging else None
            return order(tail(a), tail(b))
        else:  # non-empty lists
            print(f"{fst(a)} and {fst(b)} are non-empty lists, recurse on tail") if logging else None
            # return order([fst(a)] + [fst(a)][1:] + tail(a), [fst(b)] + [fst(b)][1:] + tail(b))
            return order([fst(fst(a))] + fst(a)[1:] + tail(a), [fst(fst(b))] + fst(b)[1:] + tail(b))

            # return order([[fst(a)][1:]] + tail(a), [[fst(b)][1:]] + tail(b))

    elif type(fst(a)) == type(fst(b)) == int:
        if fst(a) < fst(b):
            print(f"{fst(a)} < {fst(b)}, return 1") if logging else None
            return 1
        elif fst(a) > fst(b):
            print(f"{fst(a)} > {fst(b)}, return -1") if logging else None
            return -1
        else:
            print(f"{fst(a)} == {fst(b)}, return up") if logging else None
            return order(tail(a), tail(b))

    elif type(fst(a)) == list and type(fst(b)) == int:
        print(f"{fst(a)} is list but not {fst(b)}, wrap b and recurse") if logging else None
        return order(a, [[fst(b)], tail(b)])

    elif type(fst(a)) == int and type(fst(b)) == list:
        print(f"{fst(a)} is not list but {fst(b)} is, wrap a and recurse") if logging else None
        return order([[fst(a)], tail(a)], b)


data = [tuple(map(eval, pair.split("\n")))
        for pair in utils.read_input(13, 2022, test=False).split("\n\n")]

results = [(n, order(*x)) for n, x in enumerate(data)]
z = sum([fst(q) + 1 for q in results if snd(q) == 1])
print(z)

fixed = [swap(*data[n]) if snd(x) < 0 else data[n] for n, x in enumerate(results)]

data = [eval(x) for x in utils.read_input(13, 2022, test=False).split()]
data.append([[2]])
data.append([[6]])

while True:
    vals = []
    for n in range(1, len(data)):
        val = order(data[n - 1], data[n])
        vals.append(val)
        if val < 0:
            data[n - 1: n + 1] = swap(data[n - 1], data[n])

    if -1 not in vals:
        break

# data.reverse()

# while True:
#     vals = []
#     for n in range(1, len(data)):
#         val = order(data[n - 1], data[n])
#         vals.append(val)
#         if val < 0:
#             data[n - 1: n + 1] = swap(data[n - 1], data[n])
#
#     if -1 not in vals:
#         break

print((data.index([[2]]) + 1) * (data.index([[6]]) + 1))
# print([order([[1], 4], x) for x in data[:4]])
# print([order([[1], 4], x) for x in data[5:]])
# import itertools
# from itertools import islice
#
# def batched(iterable, n):
#     "Batch data into lists of length n. The last batch may be shorter."
#     # batched('ABCDEFG', 3) --> ABC DEF G
#     if n < 1:
#         raise ValueError('n must be at least one')
#     it = iter(iterable)
#     while (batch := list(islice(it, n))):
#         yield batch
#
# batches = list(batched(data, 2))

# answer 26400 is too high

# a = data[131]
# b = data[130]


a = data[130]
b = data[142]
order(a, b)
[fst(fst(a))] + fst(a)[1:] + tail(a)
[fst(fst(b))] + fst(b)[1:] + tail(b)