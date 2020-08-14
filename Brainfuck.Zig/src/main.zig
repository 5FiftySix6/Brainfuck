const std = @import("std");

const os = std.os;
const io = std.io;
const process = std.process;
const fs = std.fs;

const istream = std.io.InStream(fs.File, os.ReadError, fs.File.read);
const ostream = std.io.OutStream(fs.File, os.WriteError, fs.File.write);

const OpenFlags = fs.File.OpenFlags;

const Instr = union(enum) {
    Inc: i8,
    Shift: isize,
    Read,
    Write,
    Loop: []Instr,
};

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = &arena.allocator;

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    if (args.len != 2) {
        const stderr_file = io.getStdErr();
        var stderr_stream = stderr_file.outStream();
        try stderr_stream.print("Usage: {} FILE\n", .{args[0]});
        return 1;
    }

    var buf : [std.fs.MAX_PATH_BYTES] u8 = undefined;
    // Returns a slice of the buffer.
    const path = try os.realpath(args[1], &buf);

    const file = try fs.cwd().openFile(path, OpenFlags { .read = true, .write = false });
    defer file.close();

    const code = try allocator.alloc(u8, try file.getEndPos());
    const len = try file.read(code);

    const instrs = try parse(code, allocator);

    var mem = [_]u8{0} ** 30000;

    var stdout_file = io.getStdOut();
    var stdin_file = io.getStdIn();

    var stdout = stdout_file.outStream();
    var stdin = stdin_file.inStream();

    try interpret(instrs, &mem, &stdin, &stdout);

    return 0;
}

const ParseError = error {
    UnmatchedBracket,
    OutOfMemory
};

pub fn parse(file: []u8, allocator: *std.mem.Allocator) ParseError![]Instr {
    var instrs = std.ArrayList(Instr).init(allocator);
    errdefer instrs.deinit();

    var loop_stack: usize = 0;
    var loop_ind: usize = 0;

    var f_ind: usize = 0;

    while (f_ind < file.len) : (f_ind += 1) {
        const i = file[f_ind];

        if (loop_stack != 0) {
            switch (i) {
                '[' => loop_stack += 1,
                ']' => {
                    loop_stack -= 1;

                    if (loop_stack == 0) {
                        var inner = try parse(file[loop_ind + 1 .. f_ind], allocator);
                        try instrs.append(Instr{ .Loop = inner });
                    }
                },
                else => {},
            }

            continue;
        }

        switch (i) {
            '>', '<' => {
                var shift: isize = 0;

                while (f_ind < file.len and (file[f_ind] == '>' or file[f_ind] == '<')) : (f_ind += 1) {
                    if (file[f_ind] == '>') {
                        shift += 1;
                    } else {
                        shift -= 1;
                    }
                }

                // while loop above will increment us one over, but then so will the outer loop.
                f_ind -= 1;

                try instrs.append(Instr{ .Shift = shift });
            },
            '+', '-' => {
                var inc: i8 = 0;

                while (f_ind < file.len and (file[f_ind] == '+' or file[f_ind] == '-')) : (f_ind += 1) {
                    if (file[f_ind] == '+') {
                        inc += 1;
                    } else {
                        inc -= 1;
                    }
                }

                // while loop above will increment us one over, but then so will the outer loop.
                f_ind -= 1;

                try instrs.append(Instr{ .Inc = inc });
            },
            '.' => try instrs.append(Instr.Write),
            ',' => try instrs.append(Instr.Read),
            '[' => {
                loop_stack += 1;
                loop_ind = f_ind;
            },
            ']' => {
                return error.UnmatchedBracket;
            },
            else => {},
        }
    }

    return instrs.items;
}

const InterpretError = error{
    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    AccessDenied,
    BrokenPipe,
    SystemResources,
    OperationAborted,
    Unexpected,
    IsDir,
    WouldBlock,
    EndOfStream,
    ConnectionTimedOut,
    ConnectionResetByPeer
};

pub fn interpret(instrs: []Instr, mem: []u8, in: *istream, out: *ostream) !void {
    var m_ind: usize = 0;

    try _interpret(instrs, mem, &m_ind, in, out);
}

pub fn _interpret(instrs: []Instr, mem: []u8, m_ind: *usize, in: *istream, out: *ostream) InterpretError!void {
    for (instrs) |instr| {
        switch (instr) {
            Instr.Loop => |loop| {
                while (mem[m_ind.*] != 0) {
                    try _interpret(loop, mem, m_ind, in, out);
                }
            },
            Instr.Inc => |inc| {
                if (inc > 0) {
                    mem[m_ind.*] +%= @intCast(u8, inc);
                } else {
                    mem[m_ind.*] -%= @intCast(u8, -inc);
                }
            },
            Instr.Shift => |shift| {
                if (shift > 0) {
                    m_ind.* +%= @intCast(usize, shift);
                } else {
                    m_ind.* -%= @intCast(usize, -shift);
                }
            },
            Instr.Read => mem[m_ind.*] = try in.readByte(),
            Instr.Write => try out.print("{c}", .{mem[m_ind.*]}),
        }
    }
}
