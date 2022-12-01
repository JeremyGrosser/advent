import random
random.seed(42)

with open('day1_jumbo', 'w') as fd:
    for i in range(10_000_000):
        for j in range(random.randint(3, 10)):
            fd.write(str(random.randint(1, 8) * 1000) + '\n')
        fd.write('\n')

# The correct solution is 227000

# sha1sum day1_jumbo
# c73e8435027c2eefc5981016ca33158143210008  day1_jumbo
