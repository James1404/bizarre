const std = @import("std");
const Allocator = std.mem.Allocator;

instructions: std.ArrayList(Inst),
functions: std.ArrayList(Fn),
blocks: std.ArrayList(Block),

const Self = @This();

// An index into the instructions list
pub const Index = enum(usize) {
    _,
};

// A reference to a register
pub const Ref = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("%{d}", .{@intFromEnum(self)});
    }

    pub fn toIndex(ref: Ref) Index {
        return @enumFromInt(@intFromEnum(ref));
    }
};

// An index into the function list
pub const FnIndex = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("fn({d})", .{@intFromEnum(self)});
    }
};

// An index to a block
pub const Location = enum(usize) {
    _,
};
pub const Block = struct {
    instructions: std.ArrayList(Inst),

    pub fn deinit(block: *Block) void {
        block.instructions.deinit();
    }
};

pub const Fn = struct {
    pub const Param = struct { name: []const u8, ty: Index };

    return_type: Index,
    parameters: std.ArrayList(Param),
    body: Index,
};

pub const Inst = union(enum) {
    add: struct { lhs: Ref, rhs: Ref },
    sub: struct { lhs: Ref, rhs: Ref },
    div: struct { lhs: Ref, rhs: Ref },
    mul: struct { lhs: Ref, rhs: Ref },

    cmp_lt: struct { lhs: Ref, rhs: Ref },
    cmp_gt: struct { lhs: Ref, rhs: Ref },
    cmp_le: struct { lhs: Ref, rhs: Ref },
    cmp_ge: struct { lhs: Ref, rhs: Ref },
    cmp_eq: struct { lhs: Ref, rhs: Ref },
    cmp_ne: struct { lhs: Ref, rhs: Ref },

    negate: struct { value: Ref },

    cast: struct { value: Ref, type: Ref },

    assign: struct { value: Ref },

    load: struct { identifier: []const u8 },
    set: struct { identifier: []const u8, value: Ref },

    int: []const u8,
    float: []const u8,
    string: []const u8,
    bool: bool,
    fn_ref: FnIndex,

    push: Ref,
    pop,

    namespace: struct {
        decls: std.StringHashMap(Index),
    },
    @"struct": struct {
        fields: std.StringHashMap(Index),
        decls: std.StringHashMap(Index),
    },
    interface: struct {
        decls: std.StringHashMap(Index),
    },

    call: Ref,
    todo,

    block: Block,

    // Terminators
    @"if": struct { cond: Ref, true: Index, false: Index },
    match: struct {
        const Pattern = struct { value: Ref, branch: Index };

        value: Ref,
        patterns: std.ArrayList(Pattern),
        @"else": ?Index,
    },
    goto: Index,
    @"return",
    return_block: struct { value: Ref },

    pub fn deinit(inst: *Inst) void {
        switch (inst.*) {
            .namespace => |*n| {
                n.decls.deinit();
            },
            .@"struct" => |*n| {
                n.decls.deinit();
                n.fields.deinit();
            },
            .interface => |*n| {
                n.decls.deinit();
            },
            .match => |n| {
                n.patterns.deinit();
            },
            else => {},
        }
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .add => |v| try writer.print("{d} + {d}", .{ v.lhs, v.rhs }),
            .sub => |v| try writer.print("{d} - {d}", .{ v.lhs, v.rhs }),
            .mul => |v| try writer.print("{d} * {d}", .{ v.lhs, v.rhs }),
            .div => |v| try writer.print("{d} / {d}", .{ v.lhs, v.rhs }),

            .cmp_lt => |v| try writer.print("{d} < {d}", .{ v.lhs, v.rhs }),
            .cmp_gt => |v| try writer.print("{d} > {d}", .{ v.lhs, v.rhs }),
            .cmp_le => |v| try writer.print("{d} <= {d}", .{ v.lhs, v.rhs }),
            .cmp_ge => |v| try writer.print("{d} >= {d}", .{ v.lhs, v.rhs }),
            .cmp_eq => |v| try writer.print("{d} == {d}", .{ v.lhs, v.rhs }),
            .cmp_ne => |v| try writer.print("{d} != {d}", .{ v.lhs, v.rhs }),

            .negate => |v| try writer.print("-{d}", .{v.value}),

            .cast => |v| try writer.print("cast({d}, {d})", .{ v.value, v.type }),

            .assign => |v| try writer.print("assign({d})", .{v.value}),

            .load => |v| try writer.print("load({s})", .{v.identifier}),
            .set => |v| try writer.print("set({s}, {d})", .{ v.identifier, v.value }),

            .int => |v| try writer.print("int({s})", .{v}),
            .float => |v| try writer.print("float({s})", .{v}),
            .string => |v| try writer.print("string(\"{s}\")", .{v}),
            .bool => |v| try writer.print("bool({s})", .{if (v) "true" else "false"}),
            .fn_ref => |v| try writer.print("{s}", .{v}),

            .push => |v| try writer.print("push({s})", .{v}),
            .pop => try writer.print("pop", .{}),

            .namespace => |n| {
                try writer.print("namespace(", .{});

                var iter = n.decls.iterator();
                while (iter.next()) |kv| {
                    try writer.print("{{{d}}}", .{kv.value_ptr.*});
                }

                try writer.print(")", .{});
            },
            .@"struct" => |n| {
                try writer.print("struct(", .{});

                var iter = n.decls.iterator();
                while (iter.next()) |kv| {
                    try writer.print("{{{d}}}", .{kv.value_ptr.*});
                }

                try writer.print(")", .{});
            },
            .interface => |n| {
                try writer.print("interface(", .{});

                var iter = n.decls.iterator();
                while (iter.next()) |kv| {
                    try writer.print("{{{d}}}", .{kv.value_ptr.*});
                }

                try writer.print(")", .{});
            },

            .call => |v| try writer.print("call({s})", .{v}),

            .todo => try writer.print("todo", .{}),

            .block => |v| try writer.print("block({d})", .{v.len}),

            .@"if" => |v| try writer.print("if {s} then {d} else {d}", .{
                v.cond,
                v.true,
                v.false,
            }),
            .match => |v| {
                try writer.print("match {s} ({any})", .{ v.value, v.patterns.items });
            },
            .goto => |v| try writer.print("goto {d}", .{v}),
            .@"return" => try writer.print("return", .{}),
            .return_block => |v| try writer.print("return_block({d})", .{v.value}),
        }
    }
};

pub fn make(allocator: Allocator) Self {
    return Self{
        .instructions = .init(allocator),
        .functions = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.instructions.items) |*inst| {
        inst.deinit();
    }
    self.instructions.deinit();

    for (self.functions.items) |@"fn"| {
        @"fn".parameters.deinit();
    }
    self.functions.deinit();
}

pub fn len(self: *Self) usize {
    return self.instructions.items.len;
}

pub fn getInstruction(self: *Self, index: Index) *Inst {
    return &self.instructions.items[@intFromEnum(index)];
}
