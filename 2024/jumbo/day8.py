repeat_x = 16
repeat_y = 16
jumbo = []
with open('../input/day8', 'r') as orig:
    with open('../input/day8.jumbo', 'w') as output:
        for line in orig.readlines():
            jumbo.append(line[:-1] * repeat_x)
        for i in range(repeat_y):
            for line in jumbo:
                output.write(line)
                output.write('\n')
