use std::fs::File;
use std::io::{stdout, Read, Write};
use std::{env, mem};

use core::slice;
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

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

// We're passing it as a pointer.
#[allow(clippy::fn_to_numeric_cast)]
fn _emit(ops: &mut dynasmrt::Assembler<dynasmrt::x64::X64Relocation>, instrs: &[Instruction]) {
    //
    // rcx - buf[0]
    // rdx - buf[len]
    // r8  - buf[n]
    //
    for i in instrs {
        match i {
            Instruction::Inc(i) => {
                if *i > 0 {
                    dynasm!(ops
                        ; .arch x64
                        ; add BYTE [r8], *i
                    )
                } else {
                    dynasm!(ops
                        ; .arch x64
                        ; sub BYTE [r8], -(*i)
                    )
                }
            }
            Instruction::Ptr(shift) => {
                if *shift > 0 {
                    dynasm!(ops
                        ; .arch x64
                        ; add r8, (*shift) as i32
                    )
                } else {
                    dynasm!(ops
                        ; .arch x64
                        ; sub r8, -(*shift) as i32
                    )
                }
            }
            Instruction::Loop(sub) => {
                let start_label = ops.new_dynamic_label();
                let end_label = ops.new_dynamic_label();

                // if [r8] == 0, jmp past loop.
                // i.e. while non-zero, loop
                dynasm!(ops
                    ; .arch x64
                    ; cmp [r8], 0
                    ; je => end_label
                    ; => start_label
                );

                _emit(ops, sub);

                //  if [r8] != 0, loop.
                dynasm!(ops
                    ; .arch x64
                    ; cmp [r8], 0
                    ; jne => start_label
                    ; => end_label
                );
            }
            Instruction::Read => (),
            Instruction::Write => {
                dynasm!(ops
                    ; .arch x64
                    // Backup current cell to shadow space
                    ; mov [rsp + 0x40], r8
                    // Call
                    ; mov rax, QWORD write as _
                    ; mov rcx, r8
                    ; call rax
                    // Restore registers
                    ; mov r8,  [rsp + 0x40]
                    ; mov rdx, [rsp + 0x38]
                    ; mov rcx, [rsp + 0x30]
                )
            }
        }
    }
}

unsafe extern "win64" fn write(cell: *mut u8) {
    stdout().write_all(slice::from_raw_parts(cell, 1)).unwrap();
}

fn emit(instrs: &[Instruction]) {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();

    let start = {
        let start = ops.offset();

        dynasm!(ops
            ; .arch x64
            ; sub rsp, 0x28             // Allocate stack space (40 bytes)
            ; mov [rsp + 0x30], rcx     // Store rcx in shadow space
            ; mov [rsp + 0x38], rdx     // Store rdx in shadow space
            ; mov r8, rcx               // Set active cell to start of tape.
        );

        start
    };

    _emit(&mut ops, instrs);

    dynasm!(ops
        ; .arch x64
        ; mov rax, 0
        ; add rsp, 0x28
        ; ret
    );

    let mut buffer: [u8; 30000] = [0; 30000];

    let prog = ops.finalize().unwrap();

    let ptr = prog.as_ptr();

    let execute: extern "win64" fn(*mut u8, *mut u8) -> u8 =
        unsafe { mem::transmute(prog.ptr(start)) };

    unsafe {
        execute(buffer.as_mut_ptr(), buffer.as_mut_ptr().offset(30000));
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
        return Err(format!(
            "Got more than 2 arguments! Usage: {} FILE",
            &args[0]
        ));
    } else if args.len() == 1 {
        return Err(format!("Got no file argument! Usage: {} FILE", &args[0]));
    }

    let mut contents = String::new();

    if let Err(err) = read_file(&args[1], &mut contents) {
        return Err(format!("Unable to read file! {}", err));
    }

    emit(&parse(&contents));

    Ok(())
}
