local input = ""
local i = 1

function read_file(filename)
    local fd, err = io.open(filename, "r")
    if not fd then
        print("error opening file")
        return
    end

    local text = fd:read("*all")
    fd:close()

    input = text
end

function end_of_input()
    return (i > #input)
end

function next_char()
    if not end_of_input() then
        local ch = input:byte(i)
        i = i + 1
        return ch
    else
        return 0
    end
end

function skip_whitespace()
    while not end_of_input() do
        local ch = next_char()
        if ch ~= string.byte(' ') and 
           ch ~= string.byte('\r') and 
           ch ~= string.byte('\n')
        then
            i = i - 1
            return
        end
    end
end

function is_number(ch)
    return ch >= string.byte('0') and ch <= string.byte('9')
end

function next_num()
    local num = 0
    skip_whitespace()
    while true do
        ch = next_char()
        if is_number(ch) then
            num = (num * 10) + (ch - string.byte('0'))
        else
            break
        end
    end
    return num
end
