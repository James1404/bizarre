const std = @import("std");
const Allocator = std.mem.Allocator;
const UIR = @import("uir.zig");

registers: std.ArrayList(Value),
call_stack: std.ArrayList(usize),
pc: usize,
code: UIR,

const Self = @This();

pub fn make(allocator: Allocator, code: UIR) Self {
    return Self{
        .registers = .init(allocator),
        .call_stack = .init(allocator),
        .pc = 0,
        .code = code,
    };
}

pub fn deinit(self: *Self) void {
    self.registers.deinit();
    self.call_stack.deinit();
}

pub fn run(self: *Self) void {
    while (self.pc < self.code.len()) {
        const bytes = self.code.fetch(self.pc);
        const inst = self.code.decode(bytes);

        switch (inst.op) {
            else => {}, // TODO: Implement
        }
    }
}

pub const Value = union(enum) {
    const Index = enum(usize) { _ };

    string: []const u8,
    bool: bool,
    f32: f32,
    f64: f64,
    int: struct {
        signed: bool,
        size: enum(u8) {
            @"8",
            @"16",
            @"32",
            @"64",
        },
        bytes: []u8,
    },

    type: union(enum) {
        void,
        string,
        bool,

        i8,
        i16,
        i32,
        i64,
        isize,

        u8,
        u16,
        u32,
        u64,
        usize,

        f32,
        f64,

        ptr: UIR.Ref,
        array: struct { len: usize, type: UIR.Ref },
        slice: struct { type: UIR.Ref },

        any,
        type,
    },
};
