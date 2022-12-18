# --- Day 15: Beacon Exclusion Zone ---
#
# You feel the ground rumble again as the distress signal leads you to a large network of subterranean tunnels. You
# don't have time to search them all, but you don't need to: your pack contains a set of deployable sensors that you
# imagine were originally built to locate lost Elves.
#
# The sensors aren't very powerful, but that's okay; your handheld device indicates that you're close enough to the
# source of the distress signal to use them. You pull the emergency sensor system out of your pack, hit the big
# button on top, and the sensors zoom off down the tunnels.
#
# Once a sensor finds a spot it thinks will give it a good reading, it attaches itself to a hard surface and begins
# monitoring for the nearest signal source beacon. Sensors and beacons always exist at integer coordinates. Each
# sensor knows its own position and can determine the position of a beacon precisely; however, sensors can only lock
# on to the one beacon closest to the sensor as measured by the Manhattan distance. (There is never a tie where two
# beacons are the same distance to a sensor.)
#
# It doesn't take long for the sensors to report back their positions and closest beacons (your puzzle input). For
# example:
#
# Sensor at x=2, y=18: closest beacon is at x=-2, y=15
# Sensor at x=9, y=16: closest beacon is at x=10, y=16
# Sensor at x=13, y=2: closest beacon is at x=15, y=3
# Sensor at x=12, y=14: closest beacon is at x=10, y=16
# Sensor at x=10, y=20: closest beacon is at x=10, y=16
# Sensor at x=14, y=17: closest beacon is at x=10, y=16
# Sensor at x=8, y=7: closest beacon is at x=2, y=10
# Sensor at x=2, y=0: closest beacon is at x=2, y=10
# Sensor at x=0, y=11: closest beacon is at x=2, y=10
# Sensor at x=20, y=14: closest beacon is at x=25, y=17
# Sensor at x=17, y=20: closest beacon is at x=21, y=22
# Sensor at x=16, y=7: closest beacon is at x=15, y=3
# Sensor at x=14, y=3: closest beacon is at x=15, y=3
# Sensor at x=20, y=1: closest beacon is at x=15, y=3
#
# So, consider the sensor at 2,18; the closest beacon to it is at -2,15. For the sensor at 9,16, the closest beacon
# to it is at 10,16.
#
# Drawing sensors as S and beacons as B, the above arrangement of sensors and beacons looks like this:
#
#                1    1    2    2
#      0    5    0    5    0    5
#  0 ....S.......................
#  1 ......................S.....
#  2 ...............S............
#  3 ................SB..........
#  4 ............................
#  5 ............................
#  6 ............................
#  7 ..........S.......S.........
#  8 ............................
#  9 ............................
# 10 ....B.......................
# 11 ..S.........................
# 12 ............................
# 13 ............................
# 14 ..............S.......S.....
# 15 B...........................
# 16 ...........SB...............
# 17 ................S..........B
# 18 ....S.......................
# 19 ............................
# 20 ............S......S........
# 21 ............................
# 22 .......................B....
#
# This isn't necessarily a comprehensive map of all beacons in the area, though. Because each sensor only identifies
# its closest beacon, if a sensor detects a beacon, you know there are no other beacons that close or closer to that
# sensor. There could still be beacons that just happen to not be the closest beacon to any sensor. Consider the
# sensor at 8,7:
#
#                1    1    2    2
#      0    5    0    5    0    5
# -2 ..........#.................
# -1 .........###................
#  0 ....S...#####...............
#  1 .......#######........S.....
#  2 ......#########S............
#  3 .....###########SB..........
#  4 ....#############...........
#  5 ...###############..........
#  6 ..#################.........
#  7 .#########S#######S#........
#  8 ..#################.........
#  9 ...###############..........
# 10 ....B############...........
# 11 ..S..###########............
# 12 ......#########.............
# 13 .......#######..............
# 14 ........#####.S.......S.....
# 15 B........###................
# 16 ..........#SB...............
# 17 ................S..........B
# 18 ....S.......................
# 19 ............................
# 20 ............S......S........
# 21 ............................
# 22 .......................B....
#
# This sensor's closest beacon is at 2,10, and so you know there are no beacons that close or closer (in any
# positions marked #).
#
# None of the detected beacons seem to be producing the distress signal, so you'll need to work out where the
# distress beacon is by working out where it isn't. For now, keep things simple by counting the positions where a
# beacon cannot possibly be along just a single row.
#
# So, suppose you have an arrangement of beacons and sensors like in the example above and, just in the row where
# y=10, you'd like to count the number of positions a beacon cannot possibly exist. The coverage from all sensors
# near that row looks like this:
#
#                  1    1    2    2
#        0    5    0    5    0    5
#  9 ...#########################...
# 10 ..####B######################..
# 11 .###S#############.###########.
#
# In this example, in the row where y=10, there are 26 positions where a beacon cannot be present.
#
# Consult the report from the sensors you just deployed. In the row where y=2000000, how many positions cannot
# contain a beacon?

# --- Part Two ---
#
# Your handheld device indicates that the distress signal is coming from a beacon nearby. The distress beacon is not
# detected by any sensor, but the distress beacon must have x and y coordinates each no lower than 0 and no larger
# than 4000000.
#
# To isolate the distress beacon's signal, you need to determine its tuning frequency, which can be found by
# multiplying its x coordinate by 4000000 and then adding its y coordinate.
#
# In the example above, the search space is smaller: instead, the x and y coordinates can each be at most 20. With
# this reduced search area, there is only a single position that could have a beacon: x=14, y=11. The tuning
# frequency for this distress beacon is 56000011.
#
# Find the only possible position for the distress beacon. What is its tuning frequency?


from utils import lmap, flatten, read_input, add, fst, snd, mag, unit, manhattan, batched


def parse_readout(line):
    words = line.split()
    coords = [word.strip(",").strip(":").split("=") for word in words if "x" in word or "y" in word]
    points = batched([int(seg) for seg in flatten(coords) if seg != "x" and seg != "y"], 2)
    return lmap(tuple, points)


def points_from(a, b):
    m = mag(a, b)
    v = unit(a, b)
    points = [a]
    while m > 0:
        points.append(add(points[-1], v))
        m -= 1
    return points


def y_diff(a, y): return abs(y - snd(a))


def row_width(height, m): return abs(((2 * m) + 1) - (2 * height))


def row_scan(s, m, y, logging=False):
    height = y_diff(s, y)
    width = row_width(height, m)
    half_width = width // 2
    l, r = (fst(s) - half_width, y), (fst(s) + half_width, y)
    if height > m:
        if logging:
            print(f"For {s} with m = {m}, height = {height}, width: {width}, half width = {half_width}. L, R = {l, r}")
            print("y diff is beyond this sensor's beacon")
        return []
    # points_on_row = points_from(l, r)
    points_on_row = (l, r)
    if logging:
        print(f"For {s} with m = {m}, height = {height}, width: {width}, half width = {half_width}. L, R = {l, r}")
        print(f"{len(points_on_row)} :: {points_on_row}")
        print(f"width == # points on row? :: {width == len(points_on_row)}")
    return points_on_row


def check_continuity(point_pairs: list[tuple[tuple[int], tuple[int]]]):
    """Looks for any break in continuity within a list of pairs of x, y coordinates, where each pair specifies the
    start and end of a region within a row"""
    p = sorted(point_pairs, key=lambda x: fst(x[0]))
    for i in range(len(p) - 1):
        # if the pair range is contained within the previous, set the current as the previous
        if fst(p[i + 1][1]) < fst(p[i][1]):
            p[i + 1] = p[i]
        # x + 1 accounts for regions with touching borders
        if fst(p[i + 1][1]) > fst(p[i][1]) + 1 and fst(p[i + 1][0]) > fst(p[i][1]) + 1:
            return False, (fst(p[i][1]) + 1, snd(p[i][1]))
    return True, None


def search_unknown(search_length, sensors):
    for row in range(search_length):
        row_pairs = filter(None, [row_scan(s, sensors[s], row) for s in sensors])
        break_info = check_continuity(row_pairs)
        if not fst(break_info):
            print(f"Tuning frequency is {search_length * fst(snd(break_info)) + snd(snd(break_info))}")
            return break_info


test = False
# test = True
beacons = {fst(point): snd(point) for point in lmap(parse_readout, read_input(15, 2022, test=test).splitlines())}
distances = {sensor: manhattan(sensor, beacons[sensor]) for sensor in beacons}

row_ranges = set(flatten(filter(None, [row_scan(s, distances[s], 2000000) for s in distances])))
print(fst(max(row_ranges, key=fst)) - fst(min(row_ranges, key=fst)))
search_unknown(4_000_000, distances)
