import random
random.seed(42)

ABC = 'ABC'
XYZ = 'XYZ'

with open('day2_jumbo', 'w') as fd:
    for i in range(100_000_000):
        theirs = random.choice(ABC)
        expect = random.choice(XYZ)
        fd.write(theirs + ' ' + expect + '\n')

# The correct solution is 499992444
# sha1sum day2_jumbo
# a6d8b0994de78af8d5553ab5638cb51fc30225dc  input/day2_jumbo
