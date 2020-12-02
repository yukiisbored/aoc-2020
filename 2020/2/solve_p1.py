#!/usr/bin/env python3

def parse(line):
    bound, char, password = line.split(' ')
    low, high = bound.split('-')
    low, high = int(low), int(high)
    char = char[0]

    return (low, high, char, password)


def is_valid(low, high, char, password):
    count = sum(1 for c in password if c == char)
    return count >= low and count <= high


def main():
    res = open('in.txt', 'r').readlines()
    res = map(parse, res)
    res = map(lambda p: is_valid(*p), res)
    res = sum(1 for x in res if x)

    print(res)


if __name__ == '__main__':
    main()
