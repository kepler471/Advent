# --- Day 18: Boiling Boulders ---
#
# You and the elephants finally reach fresh air. You've emerged near the base of a large volcano that seems to be
# actively erupting! Fortunately, the lava seems to be flowing away from you and toward the ocean.
#
# Bits of lava are still being ejected toward you, so you're sheltering in the cavern exit a little longer. Outside
# the cave, you can see the lava landing in a pond and hear it loudly hissing as it solidifies.
#
# Depending on the specific compounds in the lava and speed at which it cools, it might be forming obsidian! The
# cooling rate should be based on the surface area of the lava droplets, so you take a quick scan of a droplet as it
# flies past you (your puzzle input).
#
# Because of how quickly the lava is moving, the scan isn't very good; its resolution is quite low and, as a result,
# it approximates the shape of the lava droplet with 1x1x1 cubes on a 3D grid, each given as its x,y,z position.
#
# To approximate the surface area, count the number of sides of each cube that are not immediately connected to
# another cube. So, if your scan were only two adjacent cubes like 1,1,1 and 2,1,1, each cube would have a single
# side covered and five sides exposed, a total surface area of 10 sides.
#
# Here's a larger example:
#
# 2,2,2
# 1,2,2
# 3,2,2
# 2,1,2
# 2,3,2
# 2,2,1
# 2,2,3
# 2,2,4
# 2,2,6
# 1,2,5
# 3,2,5
# 2,1,5
# 2,3,5
#
# In the above example, after counting up all the sides that aren't connected to another cube, the total surface area
# is 64.
#
# What is the surface area of your scanned lava droplet?

# --- Part Two ---
#
# Something seems off about your calculation. The cooling rate depends on exterior surface area, but your calculation
# also included the surface area of air pockets trapped in the lava droplet.
#
# Instead, consider only cube sides that could be reached by the water and steam as the lava droplet tumbles into the
# pond. The steam will expand to reach as much as possible, completely displacing any air on the outside of the lava
# droplet but never expanding diagonally.
#
# In the larger example above, exactly one cube of air is trapped within the lava droplet (at 2,2,5), so the exterior
# surface area of the lava droplet is 58.
#
# What is the exterior surface area of your scanned lava droplet?


from utils import *


# TODO: could use consecutive_groups from more_itertools here?
def count_segments(array):
    indices = np.nonzero(array)[0]
    if len(indices) == 0:
        return 0
    diffs = starmap(lambda x, y: y - x - 1, pairwise(indices))
    return len(list(filter(None, diffs))) + 1


def count_surfaces(x):
    return [2 * sum([count_segments(x[i, j, :]) for i in range(maxs[0]) for j in range(maxs[1])]),
            2 * sum([count_segments(x[i, :, k]) for i in range(maxs[0]) for k in range(maxs[2])]),
            2 * sum([count_segments(x[:, j, k]) for k in range(maxs[2]) for j in range(maxs[1])])]


# sees_wall cannot be used to determine that a point is cutoff from the outside. It will just determine whether a
# point has an uninterrupted view of the search space boundary in any of the 6 directions.
def sees_wall(x, i, j, k):
    if (
            x[i + 1:, j, k].any() and x[i, j + 1:, k].any() and x[i, j, k + 1:].any() and
            x[i::-1, j, k].any() and x[i, j::-1, k].any() and x[i, j, k::-1].any()
    ):
        return False
    return True


# mapping can be used to transform the values, eg. `lambda x: x + 10` to shift the space
def to_tensor(coords, max_indices, mapping=lambda x: x):
    x = np.zeros(list(max_indices.values()))
    for c in coords:
        x[c] = mapping(1)
    return x


data = lmap(eval, read_input(18, 2022, test=False).splitlines())
shift = 0
maxs = {index: max([n[index] for n in data]) + shift + 1 for index in range(3)}
mins = {index: min([n[index] for n in data]) + shift + 1 for index in range(3)}

obsidian = to_tensor(data, maxs)

# Find the coordinates of the air pockets
air_coords = [(i, j, k) for i in range(maxs[0]) for j in range(maxs[1]) for k in range(maxs[2]) if
              obsidian[i, j, k] == 0 and not sees_wall(obsidian, i, j, k)]

all_coords = [(i, j, k) for i in range(maxs[0]) for j in range(maxs[1]) for k in range(maxs[2])]

air = to_tensor(air_coords, maxs)

print("part 1: ", sum(count_surfaces(obsidian)))
print(len(air_coords))
print(sum(count_surfaces(air)))
print(sum(count_surfaces(obsidian)) - sum(count_surfaces(air)))

neighbours = lambda x, y, z: ((x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1))

# Brute force a graph, by finding the neighbours of all points Take the set difference of the known obsidian points,
# and then the intersection of the coordinates of the whole space - this is to remove any neighbours from outside the
# search space (eg. (0, -1, -1)). This is obviously really slow, fix it (some day)
graph = {(i, j, k): set(neighbours(i, j, k)) - set(data) & set(all_coords)
         for i in range(maxs[0]) for j in range(maxs[1]) for k in range(maxs[2])
         if (i, j, k) not in data}


def bfs(a, _graph):
    frontier = deque()
    frontier.append(a)
    visited = set(a)

    while frontier:
        current = frontier.popleft()

        for _next in _graph[current]:
            if _next not in visited:
                frontier.append(_next)
                visited.add(_next)
    return visited


v = bfs((0, 0, 0), graph)
external = set(v) | set(data)
internal = set(all_coords) - external
print("part 2: ", sum(count_surfaces(obsidian)) - sum(count_surfaces(to_tensor(internal, maxs))))
