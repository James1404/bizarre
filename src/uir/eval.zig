const std = @import("std");
const Program = @import("program.zig");
const Repr = @import("repr.zig");
const Allocator = std.mem.Allocator;

pc: usize,
program: Program,
registers: std.ArrayList(Repr.Value),
locals: std.ArrayList(Repr.Value),

const Self = @This();

pub fn make(allocator: Allocator, program: Program) Self {
    return Self{
        .pc = 0,
        .program = program,
        .registers = .init(allocator),
        .locals = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.registers.deinit();
    self.locals.deinit();
}
