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

        try decl.chunk.print(self, writer, 1);
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

    struct_decl: struct {
        dest: Ref,
        len: usize,
    },
    interface_decl: struct {
        dest: Ref,
        len: usize,
    },
    create_field: struct {
        index: usize,
        ty: Ref,
    },

    array_index: struct {
        dest: Ref,
        array: Ref,
        index: Ref,
    },

    field_access: struct {
        dest: Ref,
        value: Ref,
        field: Ref,
    },

    set_argc: struct { len: usize },
    load_arg: struct { dest: Ref, arg: usize },
    set_return_type: struct { ty: Ref },

    call: struct { dest: Ref, @"fn": Ref, args: std.ArrayList(Ref) },

    block: struct { dest: Ref, len: usize },
    comptime_block: struct { dest: Ref, len: usize },

    // control-flow
    goto: struct { loc: usize },

    loop: struct { len: usize },
    repeat,
    @"break",

    if_stmt: struct { cond: Ref, true_len: usize, false_len: usize },
    if_expr: struct { dest: Ref, cond: Ref, true_len: usize, false_len: usize },

    @"return": struct { value: Ref },
    return_implicit: struct { value: Ref },

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

pub const Chunk = struct {
    instructions: std.ArrayList(Instruction),
    register_count: usize = 0,

    pub fn make(allocator: Allocator) Chunk {
        return Chunk{
            .instructions = .init(allocator),
        };
    }

    pub fn deinit(chunk: *Chunk) void {
        for (chunk.instructions.items) |*inst| {
            inst.deinit();
        }
        chunk.instructions.deinit();
    }

    pub fn append(chunk: *Chunk, inst: Instruction) usize {
        const idx = chunk.len();
        chunk.instructions.append(inst) catch unreachable;
        return idx;
    }

    pub fn get(chunk: *Chunk, idx: usize) *Instruction {
        return &chunk.instructions.items[idx];
    }

    pub fn len(chunk: Chunk) usize {
        return chunk.instructions.items.len;
    }

    pub fn reg(chunk: *Chunk) Ref {
        const idx = chunk.register_count;
        chunk.register_count += 1;
        return @enumFromInt(idx);
    }

    pub fn print(
        chunk: *Chunk,
        self: *Self,
        writer: anytype,
        indent: usize,
    ) !void {
        for (chunk.instructions.items) |inst| {
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

                .struct_decl => |v| try writer.print(
                    "{s} = struct_decl(len: {d})",
                    .{ v.dest, v.len },
                ),
                .interface_decl => |v| try writer.print(
                    "{s} = interface_decl(len: {d})",
                    .{ v.dest, v.len },
                ),
                .create_field => |v| try writer.print(
                    "field {d} {s}",
                    .{ v.index, v.ty },
                ),

                .array_index => |v| try writer.print(
                    "{s} = {s}[{s}]",
                    .{ v.dest, v.array, v.index },
                ),
                .field_access => |v| try writer.print(
                    "{s} = {s}.{s}",
                    .{
                        v.dest,
                        v.value,
                        v.field,
                    },
                ),

                .set_argc => |v| try writer.print("set_argc({d})", .{v.len}),
                .load_arg => |v| try writer.print("{s} = load_arg({d})", .{ v.dest, v.arg }),
                .set_return_type => |v| try writer.print("set_return_type({s})", .{v.ty}),

                .call => |v| try writer.print("{s} = call({s}, {any})", .{ v.dest, v.@"fn", v.args.items }),

                .block => |v| try writer.print("{s} = block(len: {d})", .{ v.dest, v.len }),
                .comptime_block => |v| try writer.print("{s} = comptime_block(len: {d})", .{ v.dest, v.len }),

                .goto => |v| try writer.print("goto {d}", .{v.loc}),

                .loop => |v| try writer.print("loop(len: {d})", .{v.len}),
                .repeat => try writer.print("repeat", .{}),
                .@"break" => try writer.print("break", .{}),

                .if_expr => |v| try writer.print(
                    "{s} = if({s}, true_len: {d}, false_len: {d})",
                    .{ v.dest, v.cond, v.true_len, v.false_len },
                ),
                .if_stmt => |v| try writer.print(
                    "if({s}, true_len: {d}, false_len: {d})",
                    .{ v.cond, v.true_len, v.false_len },
                ),

                .@"return" => |v| try writer.print("return {s}", .{v.value}),
                .return_implicit => |v| try writer.print("return_implicit {s}", .{v.value}),
            }

            try writer.print(";\n", .{});
        }
    }
};

pub const Decl = struct {
    chunk: Chunk,
    mode: Mode,

    pub const Mode = enum { Var, Const, Fn };

    pub fn deinit(decl: *Decl) void {
        decl.chunk.deinit();
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
