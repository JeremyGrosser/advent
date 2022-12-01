import random

with open('day1_jumbo', 'w') as fd:
    for i in range(10_000_000):
        for j in range(random.randint(3, 10)):
            fd.write(str(random.randint(1, 8) * 1000) + '\n')
        fd.write('\n')
