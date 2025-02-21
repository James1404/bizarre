const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
instructions: std.ArrayList(Instruction),
constants: Constants,
bytes: std.ArrayList(u8),

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .instructions = .init(allocator),
        .constants = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.instructions.deinit();
    self.constants.deinit();
}

pub fn decode_op(self: *Self, pc: usize) OpCode {
    return std.mem.bytesAsValue(
        OpCode,
        self.bytes.items[pc .. pc + @sizeOf(OpCode)],
    );
}

pub fn decode_u32(self: *Self, pc: usize) u32 {
    return self.bytes.items[pc .. pc + @sizeOf(u32)];
}

pub fn get(self: *Self, idx: usize) *Instruction {
    return &self.instructions.items[idx];
}

pub fn len(self: *Self) usize {
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
        try writer.print("Ref.%{d}", .{@intFromEnum(self)});
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

pub const OpCode = enum(u8) {
    // Layout
    // Op A B C

    nop,

    add, // A = B + C
    sub, // A = B - C
    mul, // A = B * C
    div, // A = B / C

    cmp_lt, // A = B < C
    cmp_gt, // A = B > C
    cmp_le, // A = B <= C
    cmp_ge, // A = B >= C
    cmp_eq, // A = B == C
    cmp_ne, // A = B != C

    negative, // A = -B
    not, // A = !B

    cast, // A = B as C
    typeof, // A = typeof(B)

    move, // A = B

    load_constant, // A = constant(B)

    load, // A = vars[B]
    store, // vars[A] = B

    create_var, // vars[A] with type B
    create_const, // vars[A] with type B

    struct_decl, // A = create struct from next B instructions
    interface_decl, // A = create interface from next B instructions
    create_field, // field[A] of type B

    fn_decl, // A = create fn with No. B args from next C instructions
    namespace_decl, // A = create namespace from next B instructions

    argc, // A = No. of args
    set_arg, // arg[A] = B
    load_arg, // A = arg(B)
    set_return_type, // set return type to A

    call, // A = B(No. C args)

    block, // A = block of size B
    comptime_block, // A = comptime block of size B

    // terminators
    goto, // goto A

    loop, // loop next A instructions
    repeat, // repeat loop
    @"break", // break loop

    @"if", // if A { next B instructions } else { next C instructions }
    return_fn, // return A from fn
    return_block, // return A from block
};

pub const Instruction = packed struct {
    op: OpCode,
    a: u32,
    b: u32,
    c: u32,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.op) {
            .nop => try writer.print("nop", .{}),

            .add => try writer.print("{d} = {d} + {d}", .{ self.a, self.b, self.c }),
            .sub => try writer.print("{d} = {d} - {d}", .{ self.a, self.b, self.c }),
            .mul => try writer.print("{d} = {d} * {d}", .{ self.a, self.b, self.c }),
            .div => try writer.print("{d} = {d} + {d}", .{ self.a, self.b, self.c }),

            .cmp_lt => try writer.print("{d} = {d} < {d}", .{ self.a, self.b, self.c }),
            .cmp_gt => try writer.print("{d} = {d} > {d}", .{ self.a, self.b, self.c }),
            .cmp_le => try writer.print("{d} = {d} <= {d}", .{ self.a, self.b, self.c }),
            .cmp_ge => try writer.print("{d} = {d} >= {d}", .{ self.a, self.b, self.c }),
            .cmp_eq => try writer.print("{d} = {d} == {d}", .{ self.a, self.b, self.c }),
            .cmp_ne => try writer.print("{d} = {d} != {d}", .{ self.a, self.b, self.c }),

            .negative => try writer.print("{d} = -{d}", .{ self.a, self.b }),
            .not => try writer.print("{d} = !{d}", .{ self.a, self.b }),

            .cast => try writer.print("{d} = {d} as {d}", .{ self.a, self.b, self.c }),
            .typeof => try writer.print("{d} = typeof({d})", .{ self.a, self.b }),

            .move => try writer.print("{d} = {d}", .{ self.a, self.b }),

            .load_constant => try writer.print("{d} = constant({d})", .{ self.a, self.b }),

            .load => try writer.print("{d} = load({d})", .{ self.a, self.b }),
            .store => try writer.print("store({d}) = {d}", .{ self.a, self.b }),

            .create_var => try writer.print("create_var({d} of type {d})", .{ self.a, self.b }),
            .create_const => try writer.print("create_const({d} of type {d})", .{ self.a, self.b }),

            .struct_decl => try writer.print("{d} = struct_decl(len: {d})", .{ self.a, self.b }),
            .interface_decl => try writer.print("{d} = interface_decl(len: {d})", .{ self.a, self.b }),
            .create_field => try writer.print("create_field({d} of type {d})", .{ self.a, self.b }),

            .fn_decl => try writer.print("{d} = fn_decl(argc: {d}, len: {d})", .{ self.a, self.b, self.c }),
            .namespace_decl => try writer.print("{d} = namespace_decl(len: {d})", .{ self.a, self.b }),

            else => try writer.print("{s} :: {d} {d} {d}", .{
                @tagName(self.op),
                self.a,
                self.b,
                self.c,
            }),
        }
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
