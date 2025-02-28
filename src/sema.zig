const std = @import("std");
const Allocator = std.mem.Allocator;
const UIR = @import("uir.zig");

allocator: Allocator,
registers: []Value,
variables: []Value,
call_stack: std.ArrayList(usize),
pc: usize,
code: UIR,

const Self = @This();

pub fn make(allocator: Allocator, code: UIR) Self {
    return Self{
        .allocator = allocator,
        .registers = allocator.alloc(
            Value,
            code.registers_count,
        ) catch unreachable,
        .variables = allocator.alloc(
            Value,
            code.mapping.items.len,
        ) catch unreachable,
        .call_stack = .init(allocator),
        .pc = 0,
        .code = code,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.registers);
    self.allocator.free(self.variables);
    self.call_stack.deinit();
}

pub fn eval_chunk(self: *Self, chunk: *UIR.Chunk) void {
    while (self.pc < chunk.len()) {
        const inst = chunk.get(self.pc);

        switch (inst.*) {
            .load => |n| self.set_register(n.dest, self.get_variable(n.variable)),
            .store => |n| self.set_variable(n.variable, self.get_register(n.value)),

            else => std.debug.panic(
                "Sema: \"{s}\" not implemented",
                .{@tagName(inst.*)},
            ),
        }
    }
}

pub fn set_register(self: *Self, ref: UIR.Ref, value: Value) void {
    self.registers[@intFromEnum(ref)] = value;
}

pub fn get_register(self: *Self, ref: UIR.Ref) Value {
    return self.registers[@intFromEnum(ref)];
}

pub fn set_variable(self: *Self, variable: UIR.Variable, value: Value) void {
    self.variables[@intFromEnum(variable)] = value;
}

pub fn get_variable(self: *Self, variable: UIR.Variable) Value {
    return self.variables[@intFromEnum(variable)];
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
