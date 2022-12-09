import random
random.seed(42)


NUM_MOVES = 2_000_000


with open('day9_jumbo', 'w') as fd:
    for i in range(NUM_MOVES):
        fd.write(random.choice('UDLR') + ' ' + str(random.randint(1, 19)) + '\n')


# Part 1 solution: 12213690
# Part 2 solution: 7582117
# sha1sum day9_jumbo
# 0eb89cb3b739cf2cb325c561141a042d1eb2908f  day9_jumbo
