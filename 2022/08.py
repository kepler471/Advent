with open("input8.txt", "r") as input8:
    lines = input8.read().splitlines()


class Forest:
    def __init__(self, trees):
        self.trees = trees
        self.dims = (len(trees), len(trees[0]))

    def apply(self, fn):
        for i, row in enumerate(self.trees):
            for j, col in enumerate(row):
                self.trees[i][j] = fn(self.trees, i, j)

    def map(self, fn):
        new = [[0] * self.dims[1] for _ in range(0, self.dims[0])]

        for i, row in enumerate(self.trees):
            for j, col in enumerate(row):
                new[i][j] = fn(self.trees, i, j)

        return Forest(new)

    def reduce(self, fn):
        return fn([x for row in self.trees for x in row])

    def prettify(self, inverse=False):
        new = self.map(Forest.is_visible)
        if inverse:
            new.apply(lambda x, i, j: self.trees[i][j] if x[i][j] else "-")
        else:
            new.apply(lambda x, i, j: "-" if x[i][j] else self.trees[i][j])
        return new.__str__()

    @staticmethod
    def is_visible(x, i, j):
        row = x[i]
        col = Forest.col_to_list(x, j)

        if Forest.is_edge(x, i, j):
            return True
        elif (x[i][j] > max(row[:j]) or
              x[i][j] > max(row[j + 1:]) or
              x[i][j] > max(col[:i]) or
              x[i][j] > max(col[i + 1:])):
            return True
        return False

    @staticmethod
    def visibility(x, i, j):
        def view_distance(view):
            n = len(view)
            height = x[i][j]

            for n, t in enumerate(view):
                if t >= height:
                    break
            return n + 1

        row = x[i]
        col = Forest.col_to_list(x, j)

        return (view_distance(row[j + 1:]) *
                view_distance(list(reversed(row[:j]))) *
                view_distance(col[i + 1:]) *
                view_distance(list(reversed(col[:i]))))

    @staticmethod
    def is_edge(x, i, j):
        i_max, j_max = (len(x) - 1, len(x[0]) - 1)
        return True if i in (0, i_max) or j in (0, j_max) else False

    @staticmethod
    def col_to_list(x, j):
        return [x[i][j] for i, _ in enumerate(x)]

    def __str__(self):
        return "\n".join(["".join(map(str, row)) for row in self.trees])


forest = [list(map(int, list(row))) for row in lines]
f = Forest(forest)
print(f)
print("\n", f.prettify(inverse=True))
print("\n", f"Number of visible trees = {f.map(Forest.is_visible).reduce(sum)}")
print("\n", f"Highest scenic score in the forest = {f.map(Forest.visibility).reduce(max)}")
