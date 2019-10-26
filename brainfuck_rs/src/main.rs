use std::env;
use std::fs::File;
use std::io::{stdin, stdout, Read, Write};
use std::num::Wrapping;

#[derive(Debug)]
enum Instruction {
    Inc(i8),
    Ptr(isize),
    Loop(Vec<Instruction>),
    Read,
    Write,
}

fn parse(program: &str) -> Vec<Instruction> {
    let mut instrs = vec![];
    let mut loop_stack = 0;
    let mut loop_ind = 0;

    for (ind, i) in program.chars().enumerate() {
        if loop_stack != 0 {
            match i {
                '[' => loop_stack += 1,
                ']' => {
                    loop_stack -= 1;

                    if loop_stack == 0 {
                        instrs.push(Instruction::Loop(parse(&program[loop_ind + 1..ind])))
                    }
                }
                _ => (),
            }

            continue;
        }

        if i == '>' || i == '<' {
            if let Some(Instruction::Ptr(ref mut shift)) = instrs.last_mut() {
                *shift += match i {
                    '>' => 1,
                    '<' => -1,
                    _ => unreachable!(),
                };

                continue;
            }
        }

        if i == '+' || i == '-' {
            if let Some(Instruction::Inc(ref mut shift)) = instrs.last_mut() {
                *shift += match i {
                    '+' => 1,
                    '-' => -1,
                    _ => unreachable!(),
                };

                continue;
            }
        }

        if let Some(instr) = {
            match i {
                '>' => Some(Instruction::Ptr(1)),
                '<' => Some(Instruction::Ptr(-1)),
                '+' => Some(Instruction::Inc(1)),
                '-' => Some(Instruction::Inc(-1)),
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
            Instruction::Inc(i) => mem[*offset] += Wrapping(*i as u8),
            Instruction::Ptr(shift) => *offset = offset.wrapping_add(*shift as usize),
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
                stdout().flush().expect("Could not flush stdout.");
            }
        }
    }
}

fn read_file(f_name: &str, contents: &mut String) -> Result<(), std::io::Error> {
    let mut file = File::open(f_name)?;
    file.read_to_string(contents)?;

    Ok(())
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 3 {
        // writeln!("Got more than 2 arguments! Usage: {} FILE", &args[0]);
        return Err(format!(
            "Got more than 2 arguments! Usage: {} FILE",
            &args[0]
        ));
    } else if args.len() == 1 {
        return Err(format!(
            "Got no file argument! Usage: {} FILE [OUTPUT]",
            &args[0]
        ));
    }

    // let out = if args.len() == 2 { "a.out" } else { &args[2] };

    let mut contents = String::new();

    if let Err(err) = read_file(&args[1], &mut contents) {
        return Err(format!("Unable to read file! {}", err));
    }

    run(parse(&contents));

    Ok(())
}
