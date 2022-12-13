import utils


def fst(items): return items[0]


def snd(items): return items[1]


data = [tuple(map(eval, pair.split("\n")))
        for pair in utils.read_input(13, 2022, test=True).split("\n\n")]


def order(a, b):
    """
    return 1 for correct order, -1 for incorrect
    """
    print(f"order called for {a, b}")
    if type(fst(a)) == type(fst(b)) == list:
        if len(fst(a)) == 0 and len(fst(b)) != 0:
            print(f"{fst(a)} == 0 and {fst(b)} != 0, return 1")
            return 1
        elif len(fst(a)) != 0 and len(fst(b)) == 0:
            print(f"{fst(a)} != 0 and {fst(b)} == 0, return -1")
            return -1
        elif len(fst(a)) == 0 and len(fst(b)) == 0:
            print(f"{fst(a)} == 0 and {fst(b)} == 0, recurse on tail")
            return order(a[1:], b[1:])
        # elif fst(fst(a))
        else:  # non-empty lists
            # hove to handle if fst(a) and fst(b) are singles [1], [2]
            # have to handle if they are unequal length - [2,3,4], [4]
            # fst(a) & fst(b) are lists
            print(f"{fst(a)} and {fst(b)} are non-empty lists, recurse on tail")
            # return order([fst(a)] + [fst(a)][1:] + a[1:], [fst(b)] + [fst(b)][1:] + b[1:])
            return order([fst(fst(a))] + [fst(a)[1:]] + a[1:], [fst(fst(b))] + [fst(b)[1:]] + b[1:])

            # return order([[fst(a)][1:]] + a[1:], [[fst(b)][1:]] + b[1:])

    elif type(fst(a)) == type(fst(b)) == int:
        if fst(a) < fst(b):
            print(f"{fst(a)} < {fst(b)}, return 1")
            return 1
        elif fst(a) > fst(b):
            print(f"{fst(a)} > {fst(b)}, return -1")
            return -1
        else:
            print(f"{fst(a)} == {fst(b)}, return up")
            return order(a[1:], b[1:])

    elif type(fst(a)) == list and type(fst(b)) == int:
        print(f"{fst(a)} is list but not {fst(b)}, wrap b and recurse")
        return order(a, [b])

    elif type(fst(a)) == int and type(fst(b)) == list:
        print(f"{fst(a)} is not list but {fst(b)} is, wrap a and recurse")
        return order([a], b)


results = [order(*x) for x in order(*data[1])]
