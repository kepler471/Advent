with open("2022/input1.txt", "r") as input1:
    lines = input1.read()


inventories = [map(int, inventory.splitlines()) for inventory in lines.split("\n\n")]

calorific_weights = [sum(inventory) for inventory in inventories]

max(calorific_weights)
sum(sorted(calorific_weights)[-3:])
