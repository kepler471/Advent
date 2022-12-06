with open("2022/input4.txt", "r") as input4:
    pairs = input4.read().splitlines()

redundant_pairs = []
overlapped_pairs = []

for pair in pairs:
    pair = pair.split(",")
    print(pair)
    sections = [
        set(range(x[0], x[1] + 1))
        for x in [list(map(int, section.split("-"))) for section in pair]
    ]
    sections_ordered = sorted(sections, key=len)
    if sections_ordered[0] <= sections_ordered[1]:
        redundant_pairs.append(sections)

    if len(sections[0] & sections[1]) > 0:
        overlapped_pairs.append(sections)

len(redundant_pairs)
len(overlapped_pairs)
