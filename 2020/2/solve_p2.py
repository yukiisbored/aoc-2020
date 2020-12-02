#!/usr/bin/env python3

def read_lines(filename):
    with open('in.txt') as f:
        while True:
            buf = f.readline()

            if buf:
                yield buf
            else:
                break


def parse(line):
    [bound, char, password] = line.split(' ')
    [low, high] = bound.split('-')
    low, high = int(low), int(high)
    char = char[0]

    return (low, high, char, password)


def is_valid(low, high, char, password):
    low, high = low - 1, high - 1
    low, high = password[low] == char, password[high] == char
    return low != high


def main():
    res = read_lines('in.txt')
    res = map(parse, res)
    res = map(lambda p: is_valid(*p), res)
    res = sum(1 for x in res if x)

    print(res)


if __name__ == '__main__':
    main()
