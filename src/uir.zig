const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
bytes: std.ArrayList(u8),
constants: Constants,

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .bytes = .init(allocator),
        .constants = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.bytes.deinit();
    self.constants.deinit();
}

pub fn inc_decode_op(self: *Self, pc: *usize) OpCode {
    const size = @sizeOf(OpCode);
    const slice = self.bytes.items[pc.* .. pc.* + size];

    pc.* += size;

    return std.mem.bytesToValue(OpCode, slice);
}

pub fn inc_decode_u32(self: *Self, pc: *usize) u32 {
    const size = @sizeOf(u32);
    const slice = self.bytes.items[pc.* .. pc.* + size];

    pc.* += size;

    return std.mem.bytesToValue(u32, slice);
}

pub fn decode_op(self: *Self, pc: usize) *align(1) OpCode {
    return @ptrCast(self.bytes.items[pc .. pc + @sizeOf(OpCode)]);
}

pub fn decode_u32(self: *Self, pc: usize) *align(1) u32 {
    return @ptrCast(self.bytes.items[pc .. pc + @sizeOf(u32)]);
}

pub fn encode_op(self: *Self, op: OpCode) usize {
    const idx = self.len();
    self.bytes.appendSlice(&std.mem.toBytes(op)) catch unreachable;
    return idx;
}

pub fn encode_u32(self: *Self, v: u32) usize {
    const idx = self.len();
    self.bytes.appendSlice(&std.mem.toBytes(v)) catch unreachable;
    return idx;
}

pub fn len(self: Self) usize {
    return self.bytes.items.len;
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
    nop,

    add, // [3;u32] A = B + C
    sub, // [3;u32] A = B - C
    mul, // [3;u32] A = B * C
    div, // [3;u32] A = B / C

    cmp_lt, // [3;u32] A = B < C
    cmp_gt, // [3;u32] A = B > C
    cmp_le, // [3;u32] A = B <= C
    cmp_ge, // [3;u32] A = B >= C
    cmp_eq, // [3;u32] A = B == C
    cmp_ne, // [3;u32] A = B != C

    negative, // [2;u32] A = -B
    not, // [2;u32] A = !B

    cast, // [3;u32] A = B as C
    typeof, // [2;u32] A = typeof(B)

    move, // [2;u32] A = B

    load_constant, // [2;u32] A = constant(B)

    load, // [2;u32] A = vars[B]
    store, // [2;u32] vars[A] = B

    create_var, // [2;u32] vars[A] with type B
    create_const, // [2;u32] vars[A] with type B

    struct_decl, // [2;u32] A = create struct from next B bytes
    interface_decl, // [2;u32] A = create interface from next B bytes
    create_field, // [2;u32] field[A] of type B

    fn_decl, // [3;u32] A = create fn with No. B args from next C bytes
    namespace_decl, // [2;u32] A = create namespace from next B bytes

    argc, // [1;u32] A = No. of args
    load_arg, // [2;u32] A = arg(B)
    set_return_type, // [1;u32] set return type to A

    call, // [3 + No. C Args;u32] A = B(No. C args)

    block, // [2;u32] A = block from next B bytes
    comptime_block, // [2;u32] A = comptime block from next B bytes

    // terminators
    goto, // [1;u32] goto A

    loop, // [1;u32] loop next A bytes
    repeat, // repeat loop
    @"break", // break loop

    @"if", // [4;u32] A = if B { next C bytes } else { next D bytes }
    return_fn, // [1;u32] return A from fn
    return_block, // [1;u32] return A from block
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
