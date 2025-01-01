const std = @import("std");
const AST = @import("ast.zig");
const Token = @import("token.zig");
const Type = @import("type.zig");

pub const Loc = usize;

pub const Value = union(enum) {
    Int: []const u8,
    Float: []const u8,
    String: []const u8,
    Bool: bool,

    Type: Type.Ref,

    Ident: Token,

    Loc: Loc,

    Fn: struct {
        argc: usize,
        loc: Loc,
    },
};

pub const Inst = union(enum) {
    Nop,

    SrcLocation: struct {
        token: Token,
    },

    Jump: Loc,
    JumpIf: Loc,

    Push: Value,
    Pop,

    Add,
    Sub,
    Mul,
    Div,

    Less,
    Greater,
    Equal,
    Not,

    Store,
    Load,

    Cast,

    StoreConst,
    LoadConst,

    MakeFn,
    MakeStruct,
    MakeInterface,
    MakeField,
    MakePointer,

    Enter,
    Leave,

    Return,
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    code: std.ArrayList(Inst),

    const Self = @This();

    pub fn make(
        allocator: std.mem.Allocator,
    ) Self {
        return Self{
            .allocator = allocator,
            .code = .init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
    }

    fn append(self: *Self, inst: Inst) Loc {
        const loc = self.code.items.len;
        self.code.append(inst) catch |err| {
            @panic(@errorName(err));
        };
        return loc;
    }

    pub fn emit(self: *Self, node: AST.NodeRef) void {
        switch (node.*) {
            .Error => |_| {},

            .Binary => |n| {
                self.emit(n.rhs);
                self.emit(n.lhs);
                _ = self.append(switch (n.Op.ty) {
                    .Plus => .Add,
                    .Minus => .Sub,
                    .Asterix => .Mul,
                    .Slash => .Div,
                    else => .Nop,
                });
            },
            .Unary => |n| {
                self.emit(n.node);
                _ = self.append(switch (n.Op.ty) {
                    .Plus => .Add,
                    .Minus => .Sub,
                    .Asterix => .Mul,
                    .Slash => .Div,
                    else => .Nop,
                });
            },

            .Float => |v| _ = self.append(.{ .Push = .{ .Float = v } }),
            .Int => |v| _ = self.append(.{ .Push = .{ .Int = v } }),
            .String => |v| _ = self.append(.{ .Push = .{ .String = v } }),
            .Bool => |v| _ = self.append(.{ .Push = .{ .Bool = v } }),

            .Ident => |v| _ = self.append(.{ .Push = .{ .Ident = v } }),

            .Scope => |lst| {
                for (lst.items) |n| {
                    self.emit(n);
                }
            },

            .ConstDecl => |n| {
                self.emit(n.ident);

                _ = self.append(.StoreConst);
            },

            else => {},
        }
    }

    fn println(comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
    }

    pub fn print(self: *Self) void {
        const out = std.debug.print;

        for (self.code.items, 0..) |inst, idx| {
            out("{d} :: {s} ", .{ idx, @tagName(inst) });

            switch (inst) {
                .SrcLocation => |v| out("\"{s}\"", .{v.token.text}),

                .Jump => |v| out("{d}", .{v}),
                .JumpIf => |v| out("{d}", .{v}),

                .Push => |v| switch (v) {
                    .Int => |val| out("{s}", .{val}),
                    .Float => |val| out("{s}", .{val}),
                    .String => |val| out("{s}", .{val}),
                    .Bool => |val| out("{s}", .{
                        if (val) "true" else "false",
                    }),

                    .Type => {},

                    .Ident => |t| out("\"{s}\"", .{t.text}),

                    .Loc => |loc| out("\"{d}\"", .{loc}),

                    .Fn => |val| out("fn ({d}) at {d}", .{ val.argc, val.loc }),
                },
                else => {},
            }

            out("\n", .{});
        }
    }
};

pub const VM = struct {
    allocator: std.mem.Allocator,

    program: Program,
    pc: Loc,
    stack: std.ArrayList(Value),
    call_stack: std.ArrayList(Loc),

    const Self = @This();
    pub fn make(
        allocator: std.mem.Allocator,
        program: Program,
    ) Self {
        return Self{
            .allocator = allocator,
            .program = program,
            .pc = 0,
            .stack = .init(allocator),
            .call_stack = .init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.call_stack.deinit();
    }

    pub fn run(self: *Self) void {
        while (self.pc < self.program.code.items.len) {
            const inst = self.fetch();
            self.exec(inst);
        }
    }

    fn fetch(self: *Self) Inst {
        return self.program.code.items[self.pc];
    }

    fn exec(self: *Self, inst: Inst) void {
        switch (inst) {
            .Jump => |loc| self.pc = loc,

            .Push => |value| self.stack.append(value) catch |err| {
                @panic(@errorName(err));
            },
            .Pop => _ = self.stack.pop(),

            .Return => {
                const loc = self.call_stack.pop();
                self.pc = loc;
            },
            else => {},
        }
    }
};
