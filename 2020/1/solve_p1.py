from itertools import product


def get_numbers(filename):
    res = list()

    with open(filename, 'r') as f:
        while True:
            buf = f.readline()
            if buf:
                buf = int(buf)
                res.append(buf)
            else:
                break

    return res


def main():
    xs = get_numbers('in.txt')

    solution = next(a * b
                    for (a, b) in product(xs, xs)
                    if a + b == 2020)

    print(solution)



if __name__ == '__main__':
    main()
