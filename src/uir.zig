const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
instructions: std.ArrayList(Instruction),
constants: Constants,
registers_count: usize = 0,
variable_mapping: std.ArrayList(VariableDesc),

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .instructions = .init(allocator),
        .constants = .make(allocator),
        .variable_mapping = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.instructions.items) |*inst| {
        inst.deinit();
    }
    self.instructions.deinit();

    self.constants.deinit();
    self.variable_mapping.deinit();
}

pub fn register_variable(self: *Self, desc: VariableDesc) Variable {
    const idx = self.variable_mapping.items.len;
    self.variable_mapping.append(desc) catch unreachable;
    return @enumFromInt(idx);
}

pub fn get_variable(self: *Self, variable: Variable) []const u8 {
    return self.variable_mapping.items[@intFromEnum(variable)].name;
}

pub fn append(self: *Self, inst: Instruction) usize {
    const idx = self.len();
    self.instructions.append(inst) catch unreachable;
    return idx;
}

pub fn get(self: *Self, idx: usize) *Instruction {
    return &self.instructions.items[idx];
}

pub fn len(self: Self) usize {
    return self.instructions.items.len;
}

pub const Ref = enum(u32) {
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

// Variables names are resolved to an index
// for e.g.
// ==========
// int a = 25;
// int b = a * 2;
// ==========
//
// this gets resolved to
// ==========
// int 0 = 25;
// int 1 = var(0) * 2;
// ==========

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

    move: struct { dest: Ref, value: Ref },

    load_constant: struct { dest: Ref, index: Constants.Value.Index },
    load_builtin: struct { dest: Ref, builtin: Builtin },

    load: struct { dest: Ref, variable: Variable },
    store: struct { variable: Variable, value: Ref },

    ref: struct { dest: Ref, value: Ref },
    deref: struct { dest: Ref, value: Ref },

    create_var: struct { variable: Variable, ty: Ref },
    create_const: struct { variable: Variable, ty: Ref },

    struct_decl: struct {
        dest: Ref,
        len: usize,
    },
    interface_decl: struct {
        dest: Ref,
        len: usize,
    },
    create_field: struct {
        ident: Variable,
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
        field: Variable,
    },

    fn_decl: struct { dest: Ref, argc: usize, len: usize },
    namespace_decl: struct { dest: Ref, len: usize },

    argc: struct { dest: Ref },
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

    return_fn: struct { value: Ref },
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

pub const Variable = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{d}", .{@intFromEnum(self)});
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
        try writer.print("{s}", .{@tagName(self)});
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

pub fn print(self: *Self, writer: anytype) !void {
    try writer.print("--- Variable Mappings ---\n", .{});
    for (self.variable_mapping.items, 0..) |desc, idx| {
        try writer.print("%{d} :: {s}\n", .{ idx, desc.name });
    }
    try writer.print("-------------------------\n\n", .{});

    for (self.instructions.items) |inst| {
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
            .typeof => |v| try writer.print("{s} = typeof {s}", .{ v.dest, v.value }),

            .move => |v| try writer.print("{s} = {s}", .{ v.dest, v.value }),

            .load_constant => |v| {
                const constant = self.constants.get(v.index);
                try writer.print("{s} = {s}", .{ v.dest, constant });
            },
            .load_builtin => |v| try writer.print("{s} = {s}", .{ v.dest, v.builtin }),

            .load => |v| try writer.print("{s} = load(\"{s}\"[{s}])", .{
                v.dest,
                self.get_variable(v.variable),
                v.variable,
            }),
            .store => |v| try writer.print("{s}[{s}] = {s}", .{
                self.get_variable(v.variable),
                v.variable,
                v.value,
            }),

            .ref => |v| try writer.print("{s} = {s}.&", .{ v.dest, v.value }),
            .deref => |v| try writer.print("{s} = {s}.*", .{ v.dest, v.value }),

            .create_var => |v| try writer.print("var {s}[{s}]: {s}", .{
                self.get_variable(v.variable),
                v.variable,
                v.ty,
            }),
            .create_const => |v| try writer.print("const {s}[{s}]: {s}", .{
                self.get_variable(v.variable),
                v.variable,
                v.ty,
            }),

            .struct_decl => |v| try writer.print(
                "{s} = struct_decl(len: {d})",
                .{ v.dest, v.len },
            ),
            .interface_decl => |v| try writer.print(
                "{s} = interface_decl(len: {d})",
                .{ v.dest, v.len },
            ),
            .create_field => |v| try writer.print(
                "field {s}[{s}]: {s}",
                .{ self.get_variable(v.ident), v.ident, v.ty },
            ),

            .array_index => |v| try writer.print(
                "{s} = {s}[{s}]",
                .{ v.dest, v.array, v.index },
            ),
            .field_access => |v| try writer.print(
                "{s} = {s}.{s}[{s}]",
                .{
                    v.dest,
                    v.value,
                    self.get_variable(v.field),
                    v.field,
                },
            ),

            .fn_decl => |v| try writer.print(
                "{s} = fn_decl(argc: {d}, len: {d})",
                .{ v.dest, v.argc, v.len },
            ),
            .namespace_decl => |v| try writer.print(
                "{s} = namespace_decl(len: {d})",
                .{ v.dest, v.len },
            ),

            .argc => |v| try writer.print("{s} = argc()", .{v.dest}),
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

            .return_fn => |v| try writer.print("return_fn {s}", .{v.value}),
            .return_implicit => |v| try writer.print("return_implicit {s}", .{v.value}),
        }

        try writer.print("\n", .{});
    }
}

const VariableDesc = struct {
    name: []const u8,
    line: usize = 0,
    location: usize = 0,
};
