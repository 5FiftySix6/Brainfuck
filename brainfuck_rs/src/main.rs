use std::env;
use std::fs::File;
use std::io::{stdin, stdout, Read, Write};
use std::num::Wrapping;

#[derive(Debug)]
enum Instruction {
    Inc,
    Dec,
    Forward,
    Back,
    Loop(Vec<Instruction>),
    Read,
    Write,
}

fn parse(program: &str) -> Vec<Instruction> {
    let mut instrs = vec![];
    let mut loop_stack = 0;
    let mut loop_ind = 0;

    for (ind, i) in program.chars().enumerate() {
        if let Some(instr) = {
            if loop_stack == 0 {
                match i {
                    '>' => Some(Instruction::Forward),
                    '<' => Some(Instruction::Back),
                    '+' => Some(Instruction::Inc),
                    '-' => Some(Instruction::Dec),
                    ',' => Some(Instruction::Read),
                    '.' => Some(Instruction::Write),
                    '[' => {
                        loop_stack += 1;
                        loop_ind = ind;
                        None
                    }
                    ']' => panic!("] at {} has no matching [", ind),
                    _ => None,
                }
            } else {
                match i {
                    '[' => {
                        loop_stack += 1;
                        None
                    }
                    ']' => {
                        let start = loop_ind;

                        loop_stack -= 1;

                        if loop_stack == 0 {
                            Some(Instruction::Loop(parse(&program[start + 1..ind])))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        } {
            instrs.push(instr)
        }
    }

    instrs
}

fn run(instrs: Vec<Instruction>) {
    let mut bytes: [Wrapping<u8>; 30000] = [Wrapping(0); 30000];

    let mut offset = 0;

    run_mem(&instrs, &mut bytes, &mut offset);
}

fn run_mem(instrs: &[Instruction], mem: &mut [Wrapping<u8>; 30000], offset: &mut usize) {
    for i in instrs {
        match i {
            Instruction::Inc => mem[*offset] += Wrapping(1),
            Instruction::Dec => mem[*offset] -= Wrapping(1),
            Instruction::Forward => *offset += 1,
            Instruction::Back => *offset -= 1,
            Instruction::Loop(sub) => {
                while mem[*offset].0 != 0 {
                    run_mem(&sub, mem, offset)
                }
            }
            Instruction::Read => {
                mem[*offset] = Wrapping(
                    stdin()
                        .bytes()
                        .next()
                        .and_then(std::result::Result::ok)
                        .expect("No input!"),
                )
            }
            Instruction::Write => {
                print!("{}", mem[*offset].0 as char);
                stdout().flush().ok().expect("Could not flush stdout.");
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        panic!("Got more than 1 argument! Usage: {} FILE", &args[0]);
    } else if args.len() == 1 {
        panic!("Got no file argument! Usage: {} FILE", &args[0]);
    }

    let mut contents = String::new();
    {
        let mut file = File::open(&args[1])?;
        file.read_to_string(&mut contents)?;
    }

    run(parse(&contents));

    Ok(())
}
