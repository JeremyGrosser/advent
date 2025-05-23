require('advent_io')

function solve()
    local left = {}
    local right = {}

    while not end_of_input() do
        table.insert(left, next_number())
        table.insert(right, next_number())
    end

    table.sort(left)
    table.sort(right)

    local sum = 0
    for i, v in ipairs(left) do
        local distance = math.abs(left[i] - right[i])
        sum = sum + distance
    end

    return sum
end
