const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
declarations: std.ArrayList(Decl),
entry_point: ?Decl.Index = null,
constants: Constants,

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .declarations = .init(allocator),
        .constants = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.declarations.items) |*decl| {
        decl.deinit();
    }
    self.declarations.deinit();

    self.constants.deinit();
}

pub fn add_decl(
    self: *Self,
    decl: Decl,
) Decl.Index {
    const idx = self.declarations.items.len;
    self.declarations.append(decl) catch unreachable;
    return @enumFromInt(idx);
}

pub fn get_decl(self: *Self, index: Decl.Index) *Decl {
    return &self.declarations.items[@intFromEnum(index)];
}

pub fn print(self: *Self, writer: anytype) !void {
    for (self.declarations.items, 0..) |*decl, idx| {
        if (self.entry_point == @as(Decl.Index, @enumFromInt(idx))) {
            try writer.print("main ", .{});
        }

        try writer.print("{s} {d} {{\n", .{
            switch (decl.mode) {
                .Const => "const",
                .Var => "var",
                .Fn => "fn",
            },
            idx,
        });

        for (decl.cfg.blocks.items, 0..) |*bb, bbidx| {
            try writer.print("\tblock {d} {{\n", .{bbidx});

            try bb.print(self, writer, 2);

            try writer.print("\t}}\n\n", .{});
        }
        try writer.print("}}\n\n", .{});
    }
}

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
};

pub const Instruction = union(enum) {
    nop,

    add: struct { dest: Ref, l: Ref, r: Ref },
    sub: struct { dest: Ref, l: Ref, r: Ref },
    mul: struct { dest: Ref, l: Ref, r: Ref },
    div: struct { dest: Ref, l: Ref, r: Ref },

    cmp_lt: struct { dest: Ref, l: Ref, r: Ref },
    cmp_gt: struct { dest: Ref, l: Ref, r: Ref },
    cmp_le: struct { dest: Ref, l: Ref, r: Ref },
    cmp_ge: struct { dest: Ref, l: Ref, r: Ref },
    cmp_eq: struct { dest: Ref, l: Ref, r: Ref },
    cmp_ne: struct { dest: Ref, l: Ref, r: Ref },

    negative: struct { dest: Ref, value: Ref },
    not: struct { dest: Ref, value: Ref },

    cast: struct { dest: Ref, value: Ref, as: Ref },
    typeof: struct { dest: Ref, value: Ref },

    load_constant: struct { dest: Ref, index: Constants.Value.Index },
    load_builtin: struct { dest: Ref, builtin: Builtin },

    load: struct { dest: Ref, ref: Ref },
    store: struct { ref: Ref, value: Ref },

    load_decl: struct { dest: Ref, index: Decl.Index },
    store_decl: struct { index: Decl.Index, value: Ref },

    ref: struct { dest: Ref, value: Ref },
    deref: struct { dest: Ref, value: Ref },

    typed_value: struct { dest: Ref, value: Ref, ty: Ref },

    create_struct: struct { dest: Ref },
    create_interface: struct { dest: Ref },
    append_field: struct { decl: Ref, name: []const u8, ty: Ref },

    array_index: struct { dest: Ref, array: Ref, index: Ref },
    field_access: struct { dest: Ref, decl: Ref, name: []const u8 },

    set_argc: struct { len: usize },
    load_arg: struct { dest: Ref, arg: usize },
    set_return_type: struct { ty: Ref },

    call: struct { dest: Ref, @"fn": Ref, args: std.ArrayList(Ref) },

    fn deinit(self: *@This()) void {
        switch (self.*) {
            .call => |v| {
                v.args.deinit();
            },
            else => {},
        }
    }
};

pub const Builtin = enum {
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

    any,
    type,

    pub fn fromString(name: []const u8) ?@This() {
        const info = @typeInfo(@This()).@"enum";
        inline for (info.fields) |field| {
            if (std.mem.eql(u8, name, field.name)) {
                return @field(@This(), field.name);
            }
        }

        return null;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("Builtin.{s}", .{@tagName(self)});
    }
};

pub const Constants = struct {
    list: std.ArrayList(Value),

    pub fn make(allocator: Allocator) Constants {
        return Constants{
            .list = .init(allocator),
        };
    }

    pub fn deinit(constants: *Constants) void {
        constants.list.deinit();
    }

    pub fn append(constants: *Constants, value: Value) Value.Index {
        const index = constants.list.items.len;
        constants.list.append(value) catch unreachable;
        return @enumFromInt(index);
    }

    pub fn get(constants: *Constants, index: Value.Index) Value {
        return constants.list.items[@intFromEnum(index)];
    }

    pub const Value = union(enum) {
        pub const Index = enum(u32) {
            _,

            pub fn format(
                self: @This(),
                comptime _: []const u8,
                _: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                try writer.print("Const.%{d}", .{@intFromEnum(self)});
            }
        };

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

            ptr: Ref,
            array: struct { len: usize, type: Ref },
            slice: struct { type: Ref },

            any,
            type,
        },

        int: []const u8,
        float: []const u8,
        string: []const u8,
        bool: bool,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            switch (self) {
                .int => |v| try writer.print("int({s})", .{v}),
                .float => |v| try writer.print("float({s})", .{v}),
                .string => |v| try writer.print("string(\"{s}\")", .{v}),
                .bool => |v| try writer.print("bool({s})", .{if (v) "true" else "false"}),
                else => {},
            }
        }
    };
};

pub const Loc = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("BB.{d}", .{@intFromEnum(self)});
    }
};

pub const Terminator = union(enum) {
    const Branch = struct { pattern: Ref, loc: Loc };

    goto: Loc,
    @"if": struct { cond: Ref, true: Loc, false: Loc },
    match: struct {
        value: Ref,
        branches: std.ArrayList(Branch),
    },

    @"return": struct { value: Ref },
    return_implicit: struct { value: Ref },

    pub fn deinit(term: *Terminator) void {
        switch (term.*) {
            .match => |v| v.branches.deinit(),
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
            .goto => |v| try writer.print("goto {s}", .{v}),
            .@"if" => |v| try writer.print(
                "if {s} then {s} else {s}",
                .{ v.cond, v.true, v.false },
            ),
            .match => {},
            .@"return" => |v| try writer.print("return {s}", .{v.value}),
            .return_implicit => |v| try writer.print("return_implicit {s}", .{v.value}),
        }
    }
};

pub const BasicBlock = struct {
    instructions: std.ArrayList(Instruction),
    terminator: ?Terminator = null,
    @"comptime": bool = false,

    pub fn deinit(bb: *BasicBlock) void {
        for (bb.instructions.items) |*inst| {
            inst.deinit();
        }
        bb.instructions.deinit();
    }

    pub fn append(bb: *BasicBlock, inst: Instruction) void {
        bb.instructions.append(inst) catch unreachable;
    }

    pub fn get(bb: *BasicBlock, idx: usize) *Instruction {
        return &bb.instructions.items[idx];
    }

    pub fn print(
        bb: *BasicBlock,
        self: *Self,
        writer: anytype,
        indent: usize,
    ) !void {
        for (bb.instructions.items) |inst| {
            for (0..indent) |_| try writer.print("\t", .{});

            switch (inst) {
                .nop => try writer.print("nop", .{}),

                .add => |v| try writer.print("{s} = {s} + {s}", .{ v.dest, v.l, v.r }),
                .sub => |v| try writer.print("{s} = {s} - {s}", .{ v.dest, v.l, v.r }),
                .mul => |v| try writer.print("{s} = {s} * {s}", .{ v.dest, v.l, v.r }),
                .div => |v| try writer.print("{s} = {s} / {s}", .{ v.dest, v.l, v.r }),

                .cmp_lt => |v| try writer.print("{s} = {s} < {s}", .{ v.dest, v.l, v.r }),
                .cmp_le => |v| try writer.print("{s} = {s} <= {s}", .{ v.dest, v.l, v.r }),
                .cmp_gt => |v| try writer.print("{s} = {s} > {s}", .{ v.dest, v.l, v.r }),
                .cmp_ge => |v| try writer.print("{s} = {s} >= {s}", .{ v.dest, v.l, v.r }),
                .cmp_eq => |v| try writer.print("{s} = {s} == {s}", .{ v.dest, v.l, v.r }),
                .cmp_ne => |v| try writer.print("{s} = {s} != {s}", .{ v.dest, v.l, v.r }),

                .negative => |v| try writer.print("{s} = -{s}", .{ v.dest, v.value }),
                .not => |v| try writer.print("{s} = !{s}", .{ v.dest, v.value }),

                .cast => |v| try writer.print("{s} = {s} as {s}", .{ v.dest, v.value, v.as }),
                .typeof => |v| try writer.print("{s} = typeof({s})", .{ v.dest, v.value }),

                .load_constant => |v| {
                    const constant = self.constants.get(v.index);
                    try writer.print("{s} = {s}", .{ v.dest, constant });
                },
                .load_builtin => |v| try writer.print("{s} = {s}", .{ v.dest, v.builtin }),

                .load => |v| try writer.print("{s} = load({s})", .{
                    v.dest,
                    v.ref,
                }),
                .store => |v| try writer.print("set({s}, {s})", .{
                    v.ref,
                    v.value,
                }),

                .load_decl => |v| try writer.print("{s} = load_decl({s})", .{
                    v.dest,
                    v.index,
                }),
                .store_decl => |v| try writer.print("store_decl({s}, {s})", .{
                    v.index,
                    v.value,
                }),

                .typed_value => |v| try writer.print("{s} = typed({s}, {s})", .{
                    v.dest,
                    v.value,
                    v.ty,
                }),

                .ref => |v| try writer.print("{s} = {s}.&", .{ v.dest, v.value }),
                .deref => |v| try writer.print("{s} = {s}.*", .{ v.dest, v.value }),

                .create_struct => |v| try writer.print(
                    "{s} = create_struct()",
                    .{v.dest},
                ),
                .create_interface => |v| try writer.print(
                    "{s} = create_interface()",
                    .{v.dest},
                ),
                .append_field => |v| try writer.print(
                    "struct {s} append_field {s} {s}",
                    .{ v.decl, v.name, v.ty },
                ),

                .array_index => |v| try writer.print(
                    "{s} = {s}[{s}]",
                    .{ v.dest, v.array, v.index },
                ),
                .field_access => |v| try writer.print(
                    "{s} = {s}.\"{s}\"",
                    .{
                        v.dest,
                        v.decl,
                        v.name,
                    },
                ),

                .set_argc => |v| try writer.print("set_argc({d})", .{v.len}),
                .load_arg => |v| try writer.print("{s} = load_arg({d})", .{ v.dest, v.arg }),
                .set_return_type => |v| try writer.print("set_return_type({s})", .{v.ty}),

                .call => |v| try writer.print("{s} = call({s}, {any})", .{ v.dest, v.@"fn", v.args.items }),
            }

            try writer.print(";\n", .{});
        }
    }
};

pub const CFG = struct {
    allocator: Allocator,
    blocks: std.ArrayList(BasicBlock),
    register_count: usize = 0,

    pub fn make(allocator: Allocator) CFG {
        return CFG{
            .allocator = allocator,
            .blocks = .init(allocator),
        };
    }

    pub fn deinit(graph: *CFG) void {
        for (graph.blocks.items) |*bb| {
            bb.deinit();
        }
        graph.blocks.deinit();
    }

    pub fn append(graph: *CFG, @"comptime": bool) Loc {
        const idx = graph.blocks.items.len;
        graph.blocks.append(.{
            .instructions = .init(graph.allocator),
            .@"comptime" = @"comptime",
        }) catch unreachable;
        return @enumFromInt(idx);
    }

    pub fn get(graph: *CFG, loc: Loc) *BasicBlock {
        return &graph.blocks.items[@intFromEnum(loc)];
    }

    pub fn reg(graph: *CFG) Ref {
        const idx = graph.register_count;
        graph.register_count += 1;
        return @enumFromInt(idx);
    }
};

pub const Decl = struct {
    cfg: CFG,
    mode: Mode,

    pub const Mode = enum { Var, Const, Fn };

    pub fn deinit(decl: *Decl) void {
        decl.cfg.deinit();
    }

    pub const Index = enum(usize) {
        _,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("Decl.{d}", .{@intFromEnum(self)});
        }
    };
};

const VariableDesc = struct {
    name: []const u8,
    line: usize,
    loc: usize,
};
