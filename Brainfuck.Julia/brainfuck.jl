function Interpret(text)
    mem = Vector{UInt8}(undef, 30000)
    mem .= zero(UInt8)
    
    ptr = 1
    jmp = Vector{Int32}()
    
    i = 1
    while i <= length(text)
        c = text[i]
        if c == '>'
            ptr += 1
        elseif c == '<'
            ptr -= 1
        elseif c == '+'
            if mem[ptr] == 255
                mem[ptr] = 0
            else
                mem[ptr] += 1
            end
        elseif c == '-'
            if mem[ptr] == 0
                mem[ptr] = 255
            else
                mem[ptr] -= 1
            end
        elseif c == ','
            mem[ptr] = convert(UInt8, read(stdin, Char))
        elseif c == '.'
            print(convert(Char, mem[ptr]))
            flush(stdout)
        elseif c == '['
            if mem[ptr] != 0
                append!(jmp, i)
            else
                brackets = 1
                while brackets > 0
                    i += 1
                    
                    if text[i] == '['
                        brackets += 1
                    elseif text[i] == ']'
                        brackets -= 1
                    end
                end
            end
        elseif c == ']'
            if mem[ptr] != 0
                i = last(jmp)
            else
                pop!(jmp)
            end
        end
        
        i += 1
    end
end

if length(ARGS) != 1
    println("Supply a file parameter containing valid brainfuck code.")
    exit()
end

file = ARGS[1]

if !isfile(file)
    println("File " * file * " does not exist!")
    exit()
end

Interpret(read(file, String))
