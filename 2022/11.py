import math
import utils


class Monkey:
    def __init__(self, monkey: list[list[str]]):
        self.id = int(monkey[0][1][:-1])
        self.items = [int(n.strip(",")) for n in monkey[1][2:]]
        self.operation = lambda old: eval(" ".join(monkey[2][3:]))
        self.test = int(monkey[3][-1])
        self.true = int(monkey[4][-1])
        self.false = int(monkey[5][-1])
        self.counter = 0

    def throw_to(self, worry):
        return self.true if worry % self.test == 0 else self.false

    def __repr__(self):
        return f"Monkey - {self.id}\nitems: {self.items}\n"


def do_monkey_business(monkeys, n, reduce_worry, logging=False):
    for _ in range(n):
        for monkey in monkeys:
            for item in monkey.items:
                print(f"for monkey {monkey.id} deciding on item {item}") if logging else None
                worry = monkey.operation(item)
                print(f"worry level is {worry}, then gets lowered to {worry // 3}") if logging else None
                worry = reduce_worry(worry)
                target = monkey.throw_to(worry)
                print(f"decides to throw to {target}. monkey now holds {monkey.items}") if logging else None
                monkeys[target].items.append(worry)
                print(f"the monkey it threw to now holds items {monkeys[target].items}") if logging else None
                monkey.counter += 1
            monkey.items = []


lines = list([
    list(map(str.split, line))
    for line in map(str.splitlines, utils.read_input(11, 2022).split("\n\n"))
])

monkeys_part1 = [Monkey(monkey) for monkey in lines]
monkeys_part2 = [Monkey(monkey) for monkey in lines]
lcm = math.lcm(*[monkey.test for monkey in monkeys_part1])
do_monkey_business(monkeys_part1, 20, lambda x: x // 3, logging=False)
do_monkey_business(monkeys_part2, 10_000, lambda x: x % lcm, logging=False)
print(math.prod(sorted([m.counter for m in monkeys_part1])[-2:]))
print(math.prod(sorted([m.counter for m in monkeys_part2])[-2:]))
