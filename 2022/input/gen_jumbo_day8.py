
# random.randint is slow and we don't need high quality randomness, so just use
# a linear feedback shift register
def lfsr(seed):
    n = seed
    while n == seed:
        lsb = n & 1
        n = n >> 1
        n = n ^ ((-lsb) & 0xB400)
    return n


HEIGHT = 10_000
WIDTH = HEIGHT

# The actual Advent of Code inputs seem to be biased towards a circular shape,
# we're not gonna do that.
rand = 42
zero = ord('0')
with open('day8_jumbo', 'w') as fd:
    for y in range(HEIGHT):
        for x in range(WIDTH):
            rand = lfsr(rand)
            fd.write(chr(zero + (rand % 10)))
        fd.write('\n')

# 8.1 solution: 120253
# 8.2 solution: 620160
# sha1sum day8_jumbo
# bf1f5f2e987cd2546f699e2fd2ed3557106ec912  day8_jumbo
