require("advent_io")

local puzzle = arg[1]
require("day" .. puzzle:gsub('%.', '_'))

local filename = arg[2]
read_file(filename)

print(solve())
