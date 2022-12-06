import string

with open("2022/input3.txt", "r") as input4:
    rucksacks = input4.read().splitlines()

priorities = {letter: n + 1 for n, letter in enumerate(string.ascii_letters)}

for_rearrangement = []
badges = []

for n, sack in enumerate(rucksacks):
    mid_sack = len(sack) // 2
    comp_1, comp_2 = set(sack[:mid_sack]), set(sack[mid_sack:])
    for_rearrangement.append(comp_1 & comp_2)

    # Every 3 sacks, check for badges.
    if n % 3 == 2:
        group = list(map(set, rucksacks[n - 2 : n + 1]))
        badges.append(group[0] & group[1] & group[2])

sum([priorities["".join(item)] for item in for_rearrangement])
sum([priorities["".join(badge)] for badge in badges])
