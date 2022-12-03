import random
import string


CHARSET = string.ascii_lowercase + string.ascii_uppercase
random.seed(42)


def make_ruck(badge):
    items = list(CHARSET)
    items.remove(badge)
    random.shuffle(items)

    left = []
    right = []

    length = random.randint(12, 25)

    for i in range(length):
        left.append(items.pop())
        right.append(items.pop())

    # ensure we have exactly one duplicate item
    right[0] = left[0]

    # put the badge somewhere, don't overwrite the duplicate
    if random.getrandbits(1):
        left[1] = badge
    else:
        right[1] = badge

    # shake it up real good
    random.shuffle(right)
    random.shuffle(left)

    ruck = left + right
    return ''.join(ruck)


print('This will take a few minutes')
groups = 4_000_000
tick = groups / 100
with open('day3_jumbo', 'w') as fd:
    for i in range(groups):
        if i % tick == 0 or i == groups - 1:
            print('\033[1G', int((i + 1) / tick), '%\033[K', end='', flush=True)
        badge = random.choice(CHARSET)
        for j in range(3):
            ruck = make_ruck(badge)
            fd.write(ruck + '\n')
print('\n{:,} elves packed and ready!'.format(groups * 3))

# Part 1 solution: 318003898
# Part 2 solution: 106035719
# sha1sum day3_jumbo
# 06ad62d7691d2c9d6f41a6f78aa44628e3ee3390  day3_jumbo
