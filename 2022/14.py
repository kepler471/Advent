from matplotlib import animation
from utils import lmap, flatten, read_input, add, fst, snd, swap, mag, unit
import numpy as np
import matplotlib.pyplot as plt


def points_from(a, b):
    m = mag(a, b)
    v = unit(a, b)
    points = [a]
    while m > 0:
        points.append(add(points[-1], v))
        m -= 1
    return points


def simulate_sand(flow_source, solids, break_cond, floor_cond, plotting=False):
    y_max = max(map(snd, solids)) + 2
    if plotting:
        grid = np.zeros([y_max + 5, 400])
        grid[y_max:] = 0.33
        for solid in solids:
            grid[swap(fst(solid) - 300, snd(solid))] = 0.66  # shift x-axis towards zero
        fig, ax = plt.subplots()
        ims = []
    sand_count = 0

    while True:
        position = flow_source
        while True:
            possible = [add(position, d) for d in directions if add(position, d) not in solids]
            if not possible or floor_cond(position):
                break

            position = possible[0]  # possible move are ordered: Down, Down-Left, Down-Right

        sand_count += 1
        solids.add(position)

        if plotting and sand_count % 100 == 0:
            grid[swap(fst(position) - 300, snd(position))] = 1
            im = ax.imshow(grid, animated=True)
            ims.append([im])

        if break_cond(position):
            print(f"END SIMULATION. Sands dropped = {sand_count}")
            break

    if plotting:
        ani = animation.ArtistAnimation(fig, ims, interval=1, blit=True)
        plt.show()

    return


rock_paths = [lmap(eval, line.split(" -> ")) for line in
              read_input(14, 2022, test=False).splitlines()]

rocks = flatten(
    [flatten([points_from(path[n - 1], path[n]) for n, _ in enumerate(path) if n != 0])
     for path in rock_paths]
)

# grid = np.zeros([floor + 1, 400])
# for rock in rocks:
#     grid[swap(fst(rock) - 300, snd(rock))] = 1
# plt.imshow(grid) # Run this line to plot an image of grid

directions = ((0, 1), (-1, 1), (1, 1))
floor = max(map(snd, rocks)) + 2

simulate_sand((500, 0), set(rocks), lambda x: snd(x) == floor - 2, lambda x: snd(x) == floor - 2)
simulate_sand((500, 0), set(rocks), lambda x: x == (500, 0), lambda x: snd(x) == floor - 1)
