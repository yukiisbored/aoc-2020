#!/usr/bin/env python3

from functools import partial


def step(take_x, y):
    return (take_x * y % 31, y)


def navigate(step_func, target_y):
    return [step_func(y) for y in range(1, target_y)]


first_case = partial(navigate, partial(step, 1))


second_case = partial(navigate, partial(step, 3))


third_case = partial(navigate, partial(step, 5))


fourth_case = partial(navigate, partial(step, 7))


def fifth_case(target_y):
    return [(y // 2 % 31, y) for y in range(2, target_y, 2)]


def count_trees_of(world, navigate_func):
    points = navigate_func(len(world))
    traversed_tiles = map(lambda p: world[p[1]][p[0]], points)

    return sum(1 for tile in traversed_tiles if tile == '#')


def main():
    world = open('in.txt', 'r').readlines()
    world = [x.strip() for x in world]

    count_trees = partial(count_trees_of, world)

    first = count_trees(first_case)
    second = count_trees(second_case)
    third = count_trees(third_case)
    fourth = count_trees(fourth_case)
    fifth = count_trees(fifth_case)

    solution = first * second * third * fourth * fifth

    print(solution)


if __name__ == '__main__':
    main()
