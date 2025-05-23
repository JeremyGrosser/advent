require('advent_io')

function solve()
    local left = {}
    local right = {}

    while not end_of_input() do
        table.insert(left, next_num())
        table.insert(right, next_num())
    end

    local sum = 0
    for i, l in ipairs(left) do
        local count = 0
        for j, r in ipairs(right) do
            if l == r then
                count = count + 1
            end
        end
        sum = sum + (l * count)
    end

    return sum
end
