with open("2022/input2.txt", "r") as input2:
    guide = input2.read().splitlines()

guide = [row.split() for row in guide]

score_rules = {
    "Rock": 1,
    "Paper": 2,
    "Scissors": 3,
    "L": 0,
    "D": 3,
    "W": 6,
}

codemap = {
    "A": "Rock",
    "B": "Paper",
    "C": "Scissors",
    "X": "Rock",
    "Y": "Paper",
    "Z": "Scissors",
}


def combat(hands):
    attacker = hands[0]
    defender = hands[1]

    if attacker == defender:
        return "D"
    elif attacker == cycle(defender, "W"):
        return "W"
    else:
        return "L"


def score(hand, outcome):
    return score_rules[hand] + score_rules[outcome]


def convert_codes(game, lookup):
    attacker = lookup[game[1]]
    defender = lookup[game[0]]
    return (attacker, defender)


sum(
    [
        score(hands[0], combat(hands))
        for hands in [convert_codes(game, codemap) for game in guide]
    ]
)

new_codemap = {
    "A": "Rock",
    "B": "Paper",
    "C": "Scissors",
    "X": "L",
    "Y": "D",
    "Z": "W",
}


def cycle(hand, outcome):
    hands = ["Rock", "Paper", "Scissors"]
    outcome_map = {"W": 1, "D": 0, "L": -1}
    code = score_rules[hand] - 1
    return hands[(code + outcome_map[outcome]) % 3]


guide_decoded = [convert_codes(game, new_codemap) for game in guide]
sum(
    [
        score(hands[0], combat(hands))
        for hands in [(cycle(game[1], game[0]), game[1]) for game in guide_decoded]
    ]
)
