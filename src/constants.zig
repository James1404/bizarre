const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Index = enum(usize) { _ };
pub const Value = union(enum) {
    int_value: []const u8,
    float_value: []const u8,
    string_value: []const u8,
    bool_value: bool,
    ident: []const u8,
    fn_value: anyopaque,

    sized_int_type: struct {
        size: enum(u8) { @"8", @"16", @"32", @"64", ptrsize },
        signed: bool,
    },
    sized_float_type: struct {
        size: enum { @"32", @"64" },
    },
    string_type,
    bool_type,
    type_type,
    any_type,
    array_type: struct { child: Index, len: usize },
    pointer_type: struct { child: Index },
    fn_type: struct { args: std.ArrayList(Index), ret: Index },

    _,

    pub fn print(print_fn: anytype, constants: *Self, index: Index) void {
        switch (constants.get(index)) {
            .int_value => |v| print_fn("int({s})", .{v}),
            .float_value => |v| print_fn("float({s})", .{v}),
            .string_value => |v| print_fn("\"{s}\"", .{v}),
            .bool_value => |v| print_fn("{s}", .{
                if (v) "true" else "false",
            }),
            .ident => |v| print_fn("ident(\"{s}\")", .{v}),

            .sized_int_type => |v| print_fn("{s}{s}", .{
                if (v.signed) "i" else "s",
                switch (v.size) {
                    .@"8" => "8",
                    .@"16" => "16",
                    .@"32" => "32",
                    .@"64" => "64",
                    .ptrsize => "size",
                },
            }),
            .sized_float_type => |v| print_fn("f{s}", .{
                switch (v.size) {
                    .@"32" => "32",
                    .@"64" => "64",
                },
            }),

            .array_type => |v| {
                print_fn("[{d};", .{v.len});
                print(print_fn, constants, v.child);
                print_fn("]");
            },
            .pointer_type => |v| {
                print_fn("*");
                print(print_fn, constants, v.child);
            },

            _ => {},
        }
    }
};

allocator: Allocator,
list: std.ArrayList(Value),

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .list = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.list.deinit();
}

pub fn get(self: *Self, index: Index) Value {
    return self.list.items[@intFromEnum(index)];
}

pub fn getPtr(self: *Self, index: Index) *Value {
    return &self.list.items[@intFromEnum(index)];
}

pub fn set(self: *Self, index: Index, value: Value) void {
    const ptr = self.getPtr(index);
    ptr.* = value;
}

pub fn add(self: *Self, value: Value) Index {
    const index = self.list.items.len;
    self.list.append(value) catch |err| {
        @panic(@errorName(err));
    };

    return @enumFromInt(index);
}
