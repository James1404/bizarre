const std = @import("std");
const Allocator = std.mem.Allocator;

instructions: std.ArrayList(Inst),
constants: Constants,

// TODO: Add primitives to Ref, like: true, false, u8, u32, bool, null, etc...
// TODO: Allow refs to reference constants
// TODO: Add some kind of constant table
// TODO: Add a string interning pool

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .instructions = .init(allocator),
        .constants = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    defer self.instructions.deinit();
    for (self.instructions.items) |*inst| {
        inst.deinit();
    }

    self.constants.deinit();
}

pub fn len(self: *Self) usize {
    return self.instructions.items.len;
}

pub fn append(self: *Self, inst: Inst) Ref {
    const index = self.instructions.items.len;
    self.instructions.append(inst) catch unreachable;
    return @enumFromInt(index);
}

pub fn get(self: *Self, ref: Ref) *Inst {
    return &self.instructions.items[@intFromEnum(ref)];
}

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
        pub const Index = enum(usize) {
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

pub const Inst = union(enum) {
    nop: struct {},

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
    typeof: struct { value: Ref },

    move: struct { value: Ref },

    load_arg: struct { identifier: []const u8 },
    load_constant: struct { constant: Constants.Value.Index },

    define: struct {
        identifier: []const u8,
        type: ?Ref,
        value: ?Ref,
        mode: enum { Const, Var },
    },
    load: struct { identifier: []const u8 },
    store: struct { identifier: []const u8, value: Ref },

    block: struct { len: usize },
    comptime_block: struct { len: usize },

    match: struct {
        const Arm = struct {
            value: Ref,
            len: usize,
        };

        value: Ref,
        arms: std.ArrayList(Arm),
    },
    @"if": struct {
        cond: Ref,
        true_len: usize,
        false_len: usize,
    },
    loop: struct { len: usize },
    return_block: struct { value: Ref },
    return_fn: struct { value: Ref },
    return_fn_nothing,

    namespace_decl: struct {
        body_len: usize,
    },
    struct_decl: struct {
        const Field = struct {
            name: []const u8,
            type: Ref,
        };

        fields: std.ArrayList(Field),
        decls: std.ArrayList(Decl),
    },
    interface_decl: struct {
        decls: std.ArrayList(Decl),
    },

    fn_decl: struct {
        return_type_len: usize,
        body_len: usize,
    },

    call: struct {
        @"fn": Ref,
        args: std.ArrayList(Ref),
    },

    todo,

    pub fn deinit(inst: *Inst) void {
        switch (inst.*) {
            .struct_decl => |*n| {
                n.fields.deinit();
                n.decls.deinit();
            },
            .interface_decl => |*n| {
                n.decls.deinit();
            },
            .call => |n| {
                n.args.deinit();
            },
            else => {},
        }
    }

    pub const Decl = struct {
        name: []const u8,
        value: Ref,
    };

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .nop => try writer.print("nop", .{}),

            .add => |v| try writer.print("{s} + {s}", .{ v.lhs, v.rhs }),
            .sub => |v| try writer.print("{s} - {s}", .{ v.lhs, v.rhs }),
            .mul => |v| try writer.print("{s} * {s}", .{ v.lhs, v.rhs }),
            .div => |v| try writer.print("{s} / {s}", .{ v.lhs, v.rhs }),

            .cmp_lt => |v| try writer.print("{s} < {s}", .{ v.lhs, v.rhs }),
            .cmp_gt => |v| try writer.print("{s} > {s}", .{ v.lhs, v.rhs }),
            .cmp_le => |v| try writer.print("{s} <= {s}", .{ v.lhs, v.rhs }),
            .cmp_ge => |v| try writer.print("{s} >= {s}", .{ v.lhs, v.rhs }),
            .cmp_eq => |v| try writer.print("{s} == {s}", .{ v.lhs, v.rhs }),
            .cmp_ne => |v| try writer.print("{s} != {s}", .{ v.lhs, v.rhs }),

            .negate => |v| try writer.print("-{s}", .{v.value}),

            .cast => |v| try writer.print("cast({s}, {s})", .{ v.value, v.type }),
            .typeof => |v| try writer.print("typeof({s})", .{v.value}),

            .move => |v| try writer.print("move({s})", .{v.value}),

            .load_arg => |v| try writer.print("load_arg({s})", .{v.identifier}),
            .load_constant => |v| try writer.print("load_constant({s})", .{v.constant}),

            .define => |v| try writer.print("define({s}, {?s}, {?s}, {s})", .{
                v.identifier,
                v.type,
                v.value,
                @tagName(v.mode),
            }),

            .load => |v| try writer.print("load({s})", .{v.identifier}),
            .store => |v| try writer.print("store({s}, {s})", .{ v.identifier, v.value }),

            .namespace_decl => |n| {
                try writer.print("namespace_decl({d})", .{n.body_len});
            },
            .struct_decl => |n| {
                try writer.print("struct_decl(", .{});

                for (n.fields.items) |field| {
                    try writer.print("{s} = {s},", .{ field.name, field.type });
                }

                try writer.print(")", .{});
            },
            .interface_decl => |n| {
                try writer.print("interface_decl(", .{});

                for (n.decls.items) |field| {
                    try writer.print("{s} = {s},", .{ field.name, field.value });
                }

                try writer.print(")", .{});
            },

            .block => |v| try writer.print("block({d})", .{v.len}),
            .comptime_block => |v| try writer.print("comptime_block({d})", .{v.len}),

            .@"if" => |n| try writer.print("if({s}, {d}, {d})", .{
                n.cond,
                n.true_len,
                n.false_len,
            }),

            .call => |n| {
                try writer.print("call({s}, {{", .{n.@"fn"});
                for (n.args.items) |arg| {
                    try writer.print("{s},", .{arg});
                }
                try writer.print("}})", .{});
            },

            .todo => try writer.print("todo", .{}),

            else => try writer.print("\"{s}\" No Format Prong", .{@tagName(self)}),
        }
    }
};
