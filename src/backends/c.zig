const std = @import("std");
const Allocator = std.mem.Allocator;

allocator: Allocator,
file: std.fs.File,

const Self = @This();

pub fn make(allocator: Allocator) !Self {
    return Self{
        .allocator = allocator,
        .file = try std.fs.cwd().createFile("output.c", .{}),
    };
}

pub fn deinit(self: *Self) void {
    self.file.close();
}

pub fn header(self: *Self) void {
    self.append_include_std("stdbool.h");
    self.append_include_std("string.h");
    self.append_include_std("stdint.h");
    self.append_include_std("stdio.h");
    self.append_include_std("assert.h");
}

pub fn append_include_std(self: *Self, file: []const u8) void {
    const buf = std.fmt.allocPrint(
        self.allocator,
        "#include <{s}>\n",
        .{file},
    ) catch unreachable;
    defer self.allocator.free(buf);

    self.file.writeAll(buf) catch unreachable;
}

pub fn append_fn(
    self: *Self,
    name: []const u8,
    args: std.ArrayList(Arg),
    ret_ty: Type,
) void {
    const string: std.ArrayList(u8) = .init(self.allocator);

    {
        const buf = std.fmt.allocPrint(
            self.allocator,
            "{s} {s}(",
            .{ ret_ty, name },
        ) catch unreachable;
        defer self.allocator.free(buf);

        string.appendSlice(buf);
    }

    for (args.items, 0..) |arg, idx| {
        const buf = std.fmt.allocPrint(
            self.allocator,
            "{s} {s}",
            .{ arg.ty, arg.name },
        ) catch unreachable;
        defer self.allocator.free(buf);

        string.appendSlice(buf);

        if (idx < args.items.len) {
            string.appendSlice(", ");
        }
    }

    string.appendSlice(") {\n");
    string.appendSlice("}\n");

    const buf = string.toOwnedSlice();
    defer self.allocator.free(buf);

    self.file.writeAll(buf) catch unreachable;
}

const Arg = struct {
    name: []const u8,
    ty: Type,
};

const Type = union(enum) {
    void,
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

    array: struct { len: usize, ty: Type },
    ptr: *Type,

    @"struct": struct { name: []const u8 },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .void => try writer.print("void", .{}),
            .bool => try writer.print("bool", .{}),

            .i8 => try writer.print("int8_t", .{}),
            .i16 => try writer.print("int16_t", .{}),
            .i32 => try writer.print("int32_t", .{}),
            .i64 => try writer.print("int64_t", .{}),
            .isize => try writer.print("intptr_t", .{}),

            .u8 => try writer.print("uint8_t", .{}),
            .u16 => try writer.print("uint16_t", .{}),
            .u32 => try writer.print("uint32_t", .{}),
            .u64 => try writer.print("uint64_t", .{}),
            .usize => try writer.print("uintptr_t", .{}),

            .f32 => try writer.print("float", .{}),
            .f64 => try writer.print("double", .{}),

            .ptr => |n| try writer.print("*{s}", .{n.*}),

            .@"struct" => |n| try writer.print("struct {s}", .{n.name}),
        }
    }
};

// TODO
