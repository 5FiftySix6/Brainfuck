use std::fs::File;
use std::io::{stdin, stdout, Read, Write};
use std::{env, mem};

use core::slice;
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

#[derive(Debug)]
enum Instruction {
    Zero,
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
            Instruction::Inc(i) => dynasm!(ops
                ; .arch x64
                ; add BYTE [r8], *i
            ),
            Instruction::Ptr(shift) => dynasm!(ops
                ; .arch x64
                ; add r8, (*shift) as i32
            ),
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
            Instruction::Read => {
                dynasm!(ops
                    ; .arch x64
                    // Backup current cell to shadow space
                    ; mov [rsp + 0x40], r8
                    // Call
                    ; mov rax, QWORD read as _
                    ; mov rcx, r8
                    ; call rax
                    // Restore registers
                    ; mov r8,  [rsp + 0x40]
                    ; mov rdx, [rsp + 0x38]
                    ; mov rcx, [rsp + 0x30]
                )
            }
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
            },
            Instruction::Zero => {
                dynasm!(ops
                    ; .arch x64
                    ; mov BYTE [r8], 0
                )
             }
        }
    }
}

unsafe extern "win64" fn write(cell: *mut u8) {
    stdout().write_all(slice::from_raw_parts(cell, 1)).unwrap();
}

unsafe extern "win64" fn read(cell: *mut u8) {
    stdin()
        .read_exact(slice::from_raw_parts_mut(cell, 1))
        .unwrap();
}

fn emit(instrs: &[Instruction]) {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();

    /*
     * Stack Overview
     *
     * This is part (32 bytes) of the block we choose to allocate (0x28 = 40 bytes)
     * It's used as the shadow space for the functions we call, (read, write)
     * as dictated by the win64 calling convention
     * -------------
     * [rsp + 0x00] - .
     * [rsp + 0x08] - .
     * [rsp + 0x10] - .
     * [rsp + 0x18] - .
     *
     *
     * This isn't really part of shadow space, but we want it for alignment.
     * [rsp + 0x20] - .
     *
     * *Our* return address is stored before the shadow space of the functions we call
     *
     * [rsp + 0x28] - return address
     *
     * Shadow space (0x20 = 32 bytes) for our own function
     * -------------
     *
     * [rsp + 0x30] - buffer start
     * [rsp + 0x38] - buffer end
     * [rsp + 0x40] - current cell, used before rust calls for (,/.)
     *
     *
     * Registers
     * ----------
     * These are all volatile and have to be saved
     * before calling a function
     *
     * r8   | active cell address
     * rcx  | first param, buffer start
     * rdx  | second param, buffer end
     *
     */
    let start = {
        let start = ops.offset();

        // After a call instruction (rsp % 16) == 8.
        // The call instruction will decrement rsp by 8, thus we need to make sure we allocate
        // at least 8 bytes of stack space before making a call, even if we weren't using
        // our shadow space.
        dynasm!(ops
            ; .arch x64
            ; sub rsp, 0x28             // Allocate shadow space (32 bytes) + 8 bytes (alignment)

            // As of right now, we don't really need these as we don't do bounds checking.
            // But it's there for if we want to.
            ; mov [rsp + 0x30], rcx     // Store rcx in our shadow space
            ; mov [rsp + 0x38], rdx     // Store rdx in our shadow space

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

    let execute: extern "win64" fn(*mut u8, *mut u8) -> u8 =
        unsafe { mem::transmute(prog.ptr(start)) };

    unsafe {
        execute(buffer.as_mut_ptr(), buffer.as_mut_ptr().offset(30000));
    }
}

fn conv_sets(instrs: &mut [Instruction]) {
    for instr in instrs {
        match instr {
            Instruction::Loop(inner) => {
                if inner.len() != 1 {
                    conv_sets(inner);

                    continue
                }

                match inner[0] {
                    Instruction::Inc(_) => (),
                    _ => continue
                }

                *instr = Instruction::Zero;
            }
            _ => ()
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

    let mut instrs = parse(&contents);

    conv_sets(&mut instrs);

    emit(&instrs);

    Ok(())
}
