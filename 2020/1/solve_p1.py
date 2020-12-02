#!/usr/bin/env python3

from itertools import product

def main():
    xs = open('in.txt', 'r').readlines()
    xs = list(map(int, xs))

    solution = next(a * b
                    for (a, b) in product(xs, xs)
                    if a + b == 2020)

    print(solution)



if __name__ == '__main__':
    main()
