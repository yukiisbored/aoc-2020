#!/usr/bin/env python3

def move_to(y):
    return (3 * y % 31, y)


def navigate_to(y):
    return [move_to(y) for y in range(1, y)]


def main():
    world = open('in.txt', 'r').readlines()
    world = [x.strip() for x in world]

    points = navigate_to(len(world))
    traversed_tiles = map(lambda p: world[p[1]][p[0]], points)
    solution = sum(1 for tile in traversed_tiles if tile == '#')

    print(solution)


if __name__ == '__main__':
    main()
