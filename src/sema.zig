const std = @import("std");
const Allocator = std.mem.Allocator;
const UIR = @import("uir.zig");
const TIR = @import("tir.zig");

allocator: Allocator,
call_stack: std.ArrayList(Frame),
declarations: []?Value,
uir: UIR,
out: TIR,

const Self = @This();

pub fn make(allocator: Allocator, uir: UIR) Self {
    const self = Self{
        .allocator = allocator,
        .call_stack = .init(allocator),
        .declarations = allocator.alloc(
            ?Value,
            uir.declarations.items.len,
        ) catch unreachable,
        .uir = uir,
        .out = .make(allocator),
    };

    @memset(self.declarations, null);

    return self;
}

pub fn deinit(self: *Self) void {
    self.call_stack.deinit();
    self.allocator.free(self.declarations);
    self.out.deinit();
}

pub fn run(self: *Self) void {
    if (self.uir.entry_point) |entry| {
        self.push_frame(entry, null);
        self.eval();
    } else {
        std.debug.panic("Cannot compile program without entry point", .{});
    }
}

pub fn push_frame(
    self: *Self,
    decl: UIR.Decl.Index,
    args: ?std.ArrayList(Value),
) void {
    const reg_count = self.uir.get_decl(decl).cfg.register_count;
    self.call_stack.append(.{
        .allocator = self.allocator,
        .decl = decl,
        .args = args orelse .init(self.allocator),
        .registers = self.allocator.alloc(Value, reg_count) catch unreachable,
    }) catch unreachable;
}

pub fn pop_frame(self: *Self) void {
    const frame = &self.call_stack.items[self.call_stack.items.len - 1];
    frame.deinit();
    _ = self.call_stack.pop();
}

pub fn eval(self: *Self) void {
    while (self.call_stack.items.len > 0) {
        const frame = &self.call_stack.items[self.call_stack.items.len - 1];
        const decl = self.uir.get_decl(frame.decl);

        const block = decl.cfg.get(frame.block);
        const inst = block.get(frame.statement);
        frame.statement += 1;

        switch (inst.*) {
            .nop => std.debug.panic("nop", .{}),

            .add => |n| {
                const lhs = self.get_register(n.l);
                const rhs = self.get_register(n.r);

                self.set_register(n.dest, lhs.add(rhs));
            },
            .sub => |n| {
                const lhs = self.get_register(n.l);
                const rhs = self.get_register(n.r);

                self.set_register(n.dest, lhs.sub(rhs));
            },
            .mul => |n| {
                const lhs = self.get_register(n.l);
                const rhs = self.get_register(n.r);

                self.set_register(n.dest, lhs.mul(rhs));
            },
            .div => |n| {
                const lhs = self.get_register(n.l);
                const rhs = self.get_register(n.r);

                self.set_register(n.dest, lhs.div(rhs));
            },

            .negative => |n| {
                const value = self.get_register(n.value);

                self.set_register(n.dest, switch (value) {
                    .f32 => |v| .{ .f32 = -v },
                    .f64 => |v| .{ .f64 = -v },

                    .i8 => |v| .{ .i8 = -v },
                    .i16 => |v| .{ .i16 = -v },
                    .i32 => |v| .{ .i32 = -v },
                    .i64 => |v| .{ .i64 = -v },
                    .isize => |v| .{ .isize = -v },

                    .u8, .u16, .u32, .u64, .usize => std.debug.panic(
                        "cannot negate unsigned value of type \"{s}\"",
                        .{@tagName(value)},
                    ),

                    else => std.debug.panic(
                        "cannot negate type of \"{s}\"",
                        .{@tagName(value)},
                    ),
                });
            },
            .not => |n| {
                const value = self.get_register(n.value);

                self.set_register(n.dest, switch (value) {
                    .bool => |v| .{ .bool = !v },
                    else => std.debug.panic(
                        "cannot use not on type of \"{s}\"",
                        .{@tagName(value)},
                    ),
                });
            },

            .typeof => |n| self.set_register(
                n.dest,
                self.get_register(n.value).typeof(),
            ),

            .load_constant => |n| self.set_register(
                n.dest,
                switch (self.uir.constants.get(n.index)) {
                    .int => |v| .{ .i32 = std.fmt.parseInt(i32, v, 10) catch unreachable },
                    .float => |v| .{ .f32 = std.fmt.parseFloat(f32, v) catch unreachable },
                    .bool => |v| .{ .bool = v },
                    .string => |v| .{ .string = v },
                    .type => |ty| switch (ty) {
                        .void => .void_type,
                        else => unreachable,
                    },
                },
            ),
            .load_builtin => |n| self.set_register(
                n.dest,
                switch (n.builtin) {
                    .void => .void_type,
                    .string => .bool_type,
                    .bool => .string_type,

                    .i8 => .i8_type,
                    .i16 => .i16_type,
                    .i32 => .i32_type,
                    .i64 => .i64_type,
                    .isize => .isize_type,

                    .u8 => .u8_type,
                    .u16 => .u16_type,
                    .u32 => .u32_type,
                    .u64 => .u64_type,
                    .usize => .usize_type,

                    .f32 => .f32_type,
                    .f64 => .f64_type,

                    .any => .any_type,
                    .type => .type_type,
                },
            ),

            .load => |n| self.set_register(n.dest, self.get_register(n.ref)),
            .store => |n| self.set_register(n.ref, self.get_register(n.value)),

            .set_argc => |n| {
                frame.argc = n.len;

                if (frame.argc != frame.args.items.len) {
                    std.debug.panic(
                        "Function expected {d} arguments, but got {d}",
                        .{ frame.argc, frame.args.items.len },
                    );
                }
            },
            .load_arg => |n| self.set_register(
                n.dest,
                frame.args.items[n.arg],
            ),
            .set_return_type => |n| {
                const ty = self.get_register(n.ty);
                frame.return_type = ty;
            },

            .call => |n| {
                const @"fn" = self.get_register(n.@"fn");

                var args: std.ArrayList(Value) = .init(self.allocator);
                defer args.deinit();

                for (n.args.items) |passed_args| {
                    args.append(
                        self.get_register(passed_args),
                    ) catch unreachable;
                }

                self.push_frame(@"fn".decl, args);

                // TODO: Implement return value
            },

            else => std.debug.panic(
                "Sema: \"{s}\" not implemented",
                .{@tagName(inst.*)},
            ),
        }
    }
}

pub fn set_register(self: *Self, ref: UIR.Ref, value: Value) void {
    const frame = &self.call_stack.items[self.call_stack.items.len - 1];
    frame.registers[@intFromEnum(ref)] = value;
}

pub fn get_register(self: *Self, ref: UIR.Ref) Value {
    const frame = &self.call_stack.items[self.call_stack.items.len - 1];
    return frame.registers[@intFromEnum(ref)];
}

pub const Value = union(enum) {
    const Ref = *@This();

    string: []const u8,
    bool: bool,
    f32: f32,
    f64: f64,

    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    isize: isize,

    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    usize: usize,

    decl: UIR.Decl.Index,

    void_type,
    string_type,
    bool_type,

    i8_type,
    i16_type,
    i32_type,
    i64_type,
    isize_type,

    u8_type,
    u16_type,
    u32_type,
    u64_type,
    usize_type,

    f32_type,
    f64_type,

    ptr_type: Ref,
    array_type: struct { len: usize, type: Ref },
    slice_type: struct { type: Ref },

    any_type,
    type_type,

    pub fn deinit(
        value: *Value,
        allocator: Allocator,
    ) void {
        switch (value.*) {
            .ptr => |v| allocator.destroy(v),
            .array => |v| allocator.destroy(v.type),
            .array => |v| allocator.destroy(v.type),
            else => {},
        }
    }

    pub fn typeof(value: Value) Value {
        return switch (value) {
            .string => .string_type,
            .bool => .bool_type,
            .f32 => .f32_type,
            .f64 => .f64_type,

            .i8 => .i8_type,
            .i16 => .i16_type,
            .i32 => .i32_type,
            .i64 => .i64_type,
            .isize => .isize_type,

            .u8 => .u8_type,
            .u16 => .u16_type,
            .u32 => .u32_type,
            .u64 => .u64_type,
            .usize => .usize_type,

            else => .type_type,
        };
    }

    pub fn add(lhs: Value, rhs: Value) Value {
        const ltype = lhs.typeof();
        const rtype = rhs.typeof();

        if (!std.meta.eql(ltype, rtype)) {
            std.debug.panic(
                "Cannot add types {s} and {s}",
                .{ ltype, rtype },
            );
        }

        return switch (ltype) {
            .i8_type => .{ .i8 = lhs.i8 + rhs.i8 },
            .i16_type => .{ .i16 = lhs.i16 + rhs.i16 },
            .i32_type => .{ .i32 = lhs.i32 + rhs.i32 },
            .i64_type => .{ .i64 = lhs.i64 + rhs.i64 },
            .isize_type => .{ .isize = lhs.isize + rhs.isize },

            .u8_type => .{ .u8 = lhs.u8 + rhs.u8 },
            .u16_type => .{ .u16 = lhs.u16 + rhs.u16 },
            .u32_type => .{ .u32 = lhs.u32 + rhs.u32 },
            .u64_type => .{ .u64 = lhs.u64 + rhs.u64 },
            .usize_type => .{ .usize = lhs.usize + rhs.usize },

            .f32_type => .{ .f32 = lhs.f32 + rhs.f32 },
            .f64_type => .{ .f64 = lhs.f64 + rhs.f64 },

            else => std.debug.panic("Cannot add types of {s}", .{ltype}),
        };
    }

    pub fn sub(lhs: Value, rhs: Value) Value {
        const ltype = lhs.typeof();
        const rtype = rhs.typeof();

        if (!std.meta.eql(ltype, rtype)) {
            std.debug.panic(
                "Cannot subtract types {s} and {s}",
                .{ ltype, rtype },
            );
        }

        return switch (ltype) {
            .i8_type => .{ .i8 = lhs.i8 - rhs.i8 },
            .i16_type => .{ .i16 = lhs.i16 - rhs.i16 },
            .i32_type => .{ .i32 = lhs.i32 - rhs.i32 },
            .i64_type => .{ .i64 = lhs.i64 - rhs.i64 },
            .isize_type => .{ .isize = lhs.isize - rhs.isize },

            .u8_type => .{ .u8 = lhs.u8 - rhs.u8 },
            .u16_type => .{ .u16 = lhs.u16 - rhs.u16 },
            .u32_type => .{ .u32 = lhs.u32 - rhs.u32 },
            .u64_type => .{ .u64 = lhs.u64 - rhs.u64 },
            .usize_type => .{ .usize = lhs.usize - rhs.usize },

            .f32_type => .{ .f32 = lhs.f32 - rhs.f32 },
            .f64_type => .{ .f64 = lhs.f64 - rhs.f64 },

            else => std.debug.panic("Cannot subtract types of {s}", .{ltype}),
        };
    }

    pub fn mul(lhs: Value, rhs: Value) Value {
        const ltype = lhs.typeof();
        const rtype = rhs.typeof();

        if (!std.meta.eql(ltype, rtype)) {
            std.debug.panic(
                "Cannot multiply types {s} and {s}",
                .{ ltype, rtype },
            );
        }

        return switch (ltype) {
            .i8_type => .{ .i8 = lhs.i8 * rhs.i8 },
            .i16_type => .{ .i16 = lhs.i16 * rhs.i16 },
            .i32_type => .{ .i32 = lhs.i32 * rhs.i32 },
            .i64_type => .{ .i64 = lhs.i64 * rhs.i64 },
            .isize_type => .{ .isize = lhs.isize * rhs.isize },

            .u8_type => .{ .u8 = lhs.u8 * rhs.u8 },
            .u16_type => .{ .u16 = lhs.u16 * rhs.u16 },
            .u32_type => .{ .u32 = lhs.u32 * rhs.u32 },
            .u64_type => .{ .u64 = lhs.u64 * rhs.u64 },
            .usize_type => .{ .usize = lhs.usize * rhs.usize },

            .f32_type => .{ .f32 = lhs.f32 * rhs.f32 },
            .f64_type => .{ .f64 = lhs.f64 * rhs.f64 },

            else => std.debug.panic("Cannot multiply types of {s}", .{ltype}),
        };
    }

    pub fn div(lhs: Value, rhs: Value) Value {
        const ltype = lhs.typeof();
        const rtype = rhs.typeof();

        if (!std.meta.eql(ltype, rtype)) {
            std.debug.panic(
                "Cannot divide types {s} and {s}",
                .{ ltype, rtype },
            );
        }

        return switch (ltype) {
            .i8_type => .{ .i8 = @divExact(lhs.i8, rhs.i8) },
            .i16_type => .{ .i16 = @divExact(lhs.i16, rhs.i16) },
            .i32_type => .{ .i32 = @divExact(lhs.i32, rhs.i32) },
            .i64_type => .{ .i64 = @divExact(lhs.i64, rhs.i64) },
            .isize_type => .{ .isize = @divExact(lhs.isize, rhs.isize) },

            .u8_type => .{ .u8 = lhs.u8 / rhs.u8 },
            .u16_type => .{ .u16 = lhs.u16 / rhs.u16 },
            .u32_type => .{ .u32 = lhs.u32 / rhs.u32 },
            .u64_type => .{ .u64 = lhs.u64 / rhs.u64 },
            .usize_type => .{ .usize = lhs.usize / rhs.usize },

            .f32_type => .{ .f32 = lhs.f32 / rhs.f32 },
            .f64_type => .{ .f64 = lhs.f64 / rhs.f64 },

            else => std.debug.panic("Cannot divide types of {s}", .{ltype}),
        };
    }

    pub fn cmp_lt(lhs: Value, rhs: Value) Value {
        const ltype = lhs.typeof();
        const rtype = rhs.typeof();

        if (!std.meta.eql(ltype, rtype)) {
            std.debug.panic(
                "Cannot compare types {s} and {s}",
                .{ ltype, rtype },
            );
        }

        return switch (ltype) {
            .i8_type => .{ .i8 = lhs.i8 < rhs.i8 },
            .i16_type => .{ .i16 = lhs.i16 < rhs.i16 },
            .i32_type => .{ .i32 = lhs.i32 < rhs.i32 },
            .i64_type => .{ .i64 = lhs.i64 < rhs.i64 },
            .isize_type => .{ .isize = lhs.isize < rhs.isize },

            .u8_type => .{ .u8 = lhs.u8 < rhs.u8 },
            .u16_type => .{ .u16 = lhs.u16 < rhs.u16 },
            .u32_type => .{ .u32 = lhs.u32 < rhs.u32 },
            .u64_type => .{ .u64 = lhs.u64 < rhs.u64 },
            .usize_type => .{ .usize = lhs.usize < rhs.usize },

            .f32_type => .{ .f32 = lhs.f32 < rhs.f32 },
            .f64_type => .{ .f64 = lhs.f64 < rhs.f64 },

            else => std.debug.panic("Cannot compare types of {s}", .{ltype}),
        };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

const Frame = struct {
    allocator: Allocator,

    decl: UIR.Decl.Index,
    args: std.ArrayList(Value),
    registers: []Value,

    block: UIR.Loc = @enumFromInt(0),
    statement: usize = 0,

    argc: usize = 0,
    return_type: Value = .void_type,

    pub fn deinit(frame: *Frame) void {
        frame.args.deinit();
        frame.allocator.free(frame.registers);
    }
};
