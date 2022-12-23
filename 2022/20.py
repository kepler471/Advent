# --- Day 20: Grove Positioning System ---
#
# It's finally time to meet back up with the Elves. When you try to contact them, however, you get no reply. Perhaps
# you're out of range?
#
# You know they're headed to the grove where the star fruit grows, so if you can figure out where that is,
# you should be able to meet back up with them.
#
# Fortunately, your handheld device has a file (your puzzle input) that contains the grove's coordinates!
# Unfortunately, the file is encrypted - just in case the device were to fall into the wrong hands.
#
# Maybe you can decrypt it?
#
# When you were still back at the camp, you overheard some Elves talking about coordinate file encryption. The main
# operation involved in decrypting the file is called mixing.
#
# The encrypted file is a list of numbers. To mix the file, move each number forward or backward in the file a number
# of positions equal to the value of the number being moved. The list is circular, so moving a number off one end of
# the list wraps back around to the other end as if the ends were connected.
#
# For example, to move the 1 in a sequence like 4, 5, 6, 1, 7, 8, 9, the 1 moves one position forward: 4, 5, 6, 7, 1,
# 8, 9. To move the -2 in a sequence like 4, -2, 5, 6, 7, 8, 9, the -2 moves two positions backward, wrapping around:
# 4, 5, 6, 7, 8, -2, 9.
#
# The numbers should be moved in the order they originally appear in the encrypted file. Numbers moving around during
# the mixing process do not change the order in which the numbers are moved.
#
# Consider this encrypted file:
#
# 1
# 2
# -3
# 3
# -2
# 0
# 4
#
# Mixing this file proceeds as follows:
#
# Initial arrangement:
# 1, 2, -3, 3, -2, 0, 4
#
# 1 moves between 2 and -3:
# 2, 1, -3, 3, -2, 0, 4
#
# 2 moves between -3 and 3:
# 1, -3, 2, 3, -2, 0, 4
#
# -3 moves between -2 and 0:
# 1, 2, 3, -2, -3, 0, 4
#
# 3 moves between 0 and 4:
# 1, 2, -2, -3, 0, 3, 4
#
# -2 moves between 4 and 1:
# 1, 2, -3, 0, 3, 4, -2
#
# 0 does not move:
# 1, 2, -3, 0, 3, 4, -2
#
# 4 moves between -3 and 0:
# 1, 2, -3, 4, 0, 3, -2
#
# Then, the grove coordinates can be found by looking at the 1000th, 2000th, and 3000th numbers after the value 0,
# wrapping around the list as necessary. In the above example, the 1000th number after 0 is 4, the 2000th is -3,
# and the 3000th is 2; adding these together produces 3.
#
# Mix your encrypted file exactly once. What is the sum of the three numbers that form the grove coordinates?

from utils import lmap, read_input


def decrypt(values, key=None):
    return [v * key for v in values] if key else values


def mix(values, times=1):
    length = len(values)
    indices = list(range(length))

    for n, d in enumerate(values * times):
        i = indices.index(n % length)
        j = (i + d) % (length - 1)
        indices.insert(j if j else length, indices.pop(i))
    return [values[x] for x in indices]


data = lmap(int, read_input(20, 2022, test=False).splitlines())
result_1 = mix(decrypt(data), 1)
result_2 = mix(decrypt(data, 811589153), 10)
print(sum(result_1[(result_1.index(0) + x) % len(data)] for x in [1000, 2000, 3000]))
print(sum(result_2[(result_2.index(0) + x) % len(data)] for x in [1000, 2000, 3000]))
