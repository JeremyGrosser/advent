import random
random.seed(42)


NUM_STACKS = 32
MAX_HEIGHT = 250_000
NUM_MOVES = 250_000

stack_height = [random.randint(0, MAX_HEIGHT) for i in range(NUM_STACKS)]
MAX_HEIGHT = max(stack_height)


def select_stack():
    while True:
        stack = random.randint(0, NUM_STACKS - 1)
        if stack_height[stack] > 1:
            return stack


with open('day5_jumbo', 'w') as fd:
    for height in reversed(range(MAX_HEIGHT)):
        for stack in range(NUM_STACKS):
            if stack_height[stack] >= height:
                fd.write('[' + chr(ord('A') + random.randint(0, 25)) + ']')
            else:
                fd.write('   ')
            if stack != NUM_STACKS - 1:
                fd.write(' ')
        fd.write('\n')
    fd.write('\n')

    for i in range(NUM_MOVES):
        src = select_stack()
        while True:
            dst = select_stack()
            if dst != src:
                break
        count = random.randint(0, int(stack_height[src] / 5)) + 1
        fd.write('move {} from {} to {}\n'.format(count, src + 1, dst + 1))
        stack_height[src] -= count
        stack_height[dst] += count
