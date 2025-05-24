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

function peek_byte()
    if not end_of_input() then
        return input:byte(i)
    else
        return 0
    end
end

function next_byte()
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
        local ch = peek_byte()
        if ch == string.byte(' ') or
           ch == string.byte('\r') or
           ch == string.byte('\n')
        then
            i = i + 1
        else
            return
        end
    end
end

function is_number(ch)
    return ch >= string.byte('0') and ch <= string.byte('9')
end

function next_number()
    local num = 0
    skip_whitespace()
    while true do
        local ch = peek_byte()
        if is_number(ch) then
            num = (num * 10) + (ch - string.byte('0'))
            i = i + 1
        else
            break
        end
    end
    return num
end
