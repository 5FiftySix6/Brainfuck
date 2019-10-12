import sys
from os import path
from typing import List

def Interpret(text : str) -> None:
    mem : List[byte] = [0] * 30000
    ptr : int = 0
    
    jmp : List[int] = []
    
    i : int = 0
    while i < len(text):
        c : char = text[i]
        
        if c == '>':
            ptr += 1
        elif c == '<':
            ptr -= 1
        elif c == '+':
            mem[ptr] = mem[ptr] + 1
        elif c == '-':
            mem[ptr] = mem[ptr] - 1
        elif c == ',':
            mem[ptr] = ord(sys.stdin.read(1))
        elif c == '.':
            print(chr(mem[ptr]), end="")
            sys.stdout.flush()
        elif c == '[':
            if mem[ptr] != 0:
                jmp.append(i)
            else:
                brackets : int = 1
                
                while brackets > 0:
                    i += 1
                    
                    if text[i] == '[':
                        brackets += 1
                    elif text[i] == ']':
                        brackets -= 1
        elif c == ']':
            if mem[ptr] != 0:
                i = jmp[-1]
            else:
                jmp.pop()
                
        i += 1

if not len(sys.argv) == 2:
    print("Supply a file parameter containing valid brainfuck code.")
    sys.exit()
    
file : str = sys.argv[1]

if not path.exists(file):
    print("File " + file + " does not exist!")
    print("Exiting.")
    sys.exit()

with open(file) as f:
    text : str = "".join(f.readlines())
    Interpret(text)