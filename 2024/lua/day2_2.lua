require('advent_io')

function all_increasing(report)
    for i = 2, #report do
        if report[i] <= report[i-1] then
            return false
        end
    end
    return true
end

function all_decreasing(report)
    for i = 2, #report do
        if report[i] >= report[i-1] then
            return false
        end
    end
    return true
end

function check_difference(report)
    for i = 2, #report do
        local diff = math.abs(report[i] - report[i-1]) 
        if diff < 1 or diff > 3 then
            return false
        end
    end
    return true
end

function shallow_copy(original)
    local copy = {}
    for k, v in pairs(original) do
        copy[k] = v
    end
    return copy
end

function check_report(report)
    if (all_increasing(report) or all_decreasing(report)) and check_difference(report) then
        return true
    else
        for i, _ in ipairs(report) do
            local trial = shallow_copy(report)
            table.remove(trial, i)
            if (all_increasing(trial) or all_decreasing(trial)) and check_difference(trial) then
                return true
            end
        end
        return false
    end
end

function solve()
    local report = {}
    local safe = 0
    while not end_of_input() do
        if peek_char() == string.byte('\n') then
            if check_report(report) then
                safe = safe + 1
            end
            report = {}
            skip_whitespace()
        else
            table.insert(report, next_number())
        end
    end
    return safe
end
