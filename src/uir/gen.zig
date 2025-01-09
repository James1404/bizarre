const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../ast.zig");
const Repr = @import("repr.zig");
const BlockList = @import("blocklist.zig");
const Program = @import("program.zig");

allocator: Allocator,

// No need to deinit this, it's passed to a program as an owned slice!
blocks: BlockList,
current: BlockList.Location,

const Self = @This();

fn make(
    allocator: Allocator,
) Self {
    var self = Self{
        .allocator = allocator,
        .blocks = .make(allocator),
        .current = 0,
    };

    self.current = self.blocks.append(.make(allocator));

    return self;
}

fn deinit(_: *Self) void {}

fn down(self: *Self) BlockList.Location {
    const current = self.blocks.get(self.current);

    const child = BlockList.Block.make(self.allocator);
    const child_loc = self.blocks.append(child);

    current.children.append(child_loc) catch |err| {
        @panic(@errorName(err));
    };

    self.current = child_loc;
    return child_loc;
}

fn up(self: *Self) void {
    if (self.blocks.get(self.current).parent) |parent| {
        self.current = parent;
    }
}

fn temp(self: *Self) Repr.Ref {
    const ptr = &self.blocks.get(self.current).temp_counter;
    const idx = ptr.*;
    ptr.* += 1;
    return idx;
}

fn append(self: *Self, inst: Repr.Inst) void {
    self.blocks.get(self.current).append(inst);
}

fn append_value(self: *Self, value: Repr.Value.Data) Repr.Ref {
    const addr = self.temp();
    self.append(.{ .value = .{
        .addr = addr,
        .value = .make_untyped(value),
    } });
    return addr;
}

fn emit(self: *Self, node: AST.NodeRef) ?Repr.Ref {
    std.log.info("Emitting: {s}", .{@tagName(node.*)});
    switch (node.*) {
        .Scope => |lst| {
            _ = self.down();
            for (lst.items) |n| {
                _ = self.emit(n);
            }
        },
        .Binary => |v| {
            const l = self.emit(v.lhs).?;
            const r = self.emit(v.lhs).?;
            const addr = self.temp();

            switch (v.Op.ty) {
                .Plus => self.append(.{ .add = .{
                    .addr = addr,
                    .lhs = l,
                    .rhs = r,
                } }),
                .Minus => self.append(.{ .sub = .{
                    .addr = addr,
                    .lhs = l,
                    .rhs = r,
                } }),
                .Asterix => self.append(.{ .mul = .{
                    .addr = addr,
                    .lhs = l,
                    .rhs = r,
                } }),
                .Slash => self.append(.{ .div = .{
                    .addr = addr,
                    .lhs = l,
                    .rhs = r,
                } }),
                else => @panic("Unknown binary operator"),
            }

            return addr;
        },
        .ConstDecl => |n| {
            const id = self.emit(n.ident).?;
            const val = self.emit(n.value).?;
            self.append(.{ .assign = .{ .addr = id, .value = val } });
        },
        .Assignment => |v| {
            const id = self.emit(v.ident).?;
            const val = self.emit(v.value).?;
            self.append(.{ .assign = .{ .addr = id, .value = val } });
        },
        .String => |v| return self.append_value(.{ .string = v }),
        .Int => |v| return self.append_value(.{ .int = v }),
        .Float => |v| return self.append_value(.{ .float = v }),
        .Bool => |v| return self.append_value(.{ .bool = v }),
        .Ident => |v| return self.append_value(.{ .ident = v.text }),
        .FnDecl => |v| {
            const loc = self.down();

            self.append(.begin_function);

            _ = self.emit(v.params);
            _ = self.emit(v.block);

            self.append(.end_function);

            self.up();

            return loc;
        },

        .ParamaterList => |lst| {
            for (lst.items) |param| _ = self.emit(param);
        },
        .Paramater => |v| {
            const loc = self.temp();
            self.append(.{ .pop_param = .{
                .addr = loc,
            } });

            self.append(.{ .cast = .{
                .addr = self.temp(),
                .value = loc,
                .type = self.emit(v.type).?,
            } });
        },

        .Return => |v| self.append(.{ .@"return" = self.emit(v).? }),

        else => std.log.err("Tag '{s}' not implemented", .{@tagName(node.*)}),
    }

    return null;
}

pub fn generate(allocator: Allocator, ast: AST) Program {
    var gen = Self.make(allocator);
    defer gen.deinit();

    if (ast.root) |root| {
        _ = gen.emit(root);
    }

    return Program{
        .allocator = allocator,
        .blocks = gen.blocks,
    };
}
