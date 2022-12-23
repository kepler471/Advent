from utils import *
from sklearn.linear_model import LinearRegression


def parse(line):
    line = line.strip(":").split()
    match line:
        case [name, number]:
            return "val", name.strip(":"), int(number)
        case [name, *equation]:
            return "exp", name.strip(":"), equation


def yelling(num, root_op):
    values = {d[1]: d[2] for d in data if d[0] == "val"}
    expressions = {d[1]: d[2] for d in data if d[0] == "exp"}
    expressions["root"] = [expressions["root"][0], root_op, expressions["root"][2]]
    values["humn"] = num
    while expressions:
        deletion = []
        for exp in expressions:
            exp_1, operator, exp_2 = expressions[exp]
            if exp_1 in values and exp_2 in values:
                values[exp] = eval(f"values[exp_1] {operator} values[exp_2]")
                deletion.append(exp)
        for d in deletion:
            del expressions[d]

    return values["root"]


data = lmap(parse, read_input(21, 2022, test=False).splitlines())
# The only variable is "humn", so this can be solved with linear regression
model = LinearRegression()
model.fit(x := np.linspace(1, 10e12, 101).reshape((-1, 1)), y := yelling(x, "-"))
print("Yell the number: ", int(x_0 := - model.intercept_ / model.coef_))
