with open("2022/input5.txt", "r") as input5:
    lines = input5.read().splitlines()

state = list(reversed(lines[:8]))
stack_ids = lines[8].split()
instructions = lines[10:]
img_locs = [x * 4 + 1 for x in range(0, 9)]
transverse_stacks = [
    [state[row][col] for col in img_locs if col < len(state[row])]
    for row, _ in enumerate(state)
]


def parse_instr(instr):
    instr = instr.split()
    return {"n": int(instr[1]), "from": int(instr[3]), "to": int(instr[5])}


def move_crates(stacks, instr, single=True):
    lifted = stacks[instr["from"] - 1][-instr["n"] :]
    lifted = reversed(lifted) if single else lifted

    del stacks[instr["from"] - 1][-instr["n"] :]
    stacks[instr["to"] - 1] += lifted


def rearrange_crates(crates, single=True):
    stacks = [[] for _ in stack_ids]
    _ = [
        [stacks[col].append(char) for col, char in enumerate(row) if char != " "]
        for row in crates
    ]
    _ = [move_crates(stacks, parse_instr(i)) for i in instructions]
    _ = [move_crates(stacks, parse_instr(i), single=single) for i in instructions]

    return crates
