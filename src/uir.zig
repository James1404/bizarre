const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("ast.zig");

const Scopes = @import("scopes.zig");

pub const Code = struct {
    instructions: []Inst,
    blocks: []BasicBlock,

    pub const BasicBlock = struct {
        start: usize,
        len: usize,
    };
};

pub const Inst = union(enum) {
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

    assign: struct { value: Ref },

    load: struct { identifier: []const u8 },
    set: struct { identifier: []const u8, value: Ref },

    int: []const u8,
    float: []const u8,
    string: []const u8,
    bool: bool,
    fn_ref: FnIndex,

    push: Ref,
    pop: struct { addr: Ref },

    create_struct,
    create_interface,

    call: Ref,
    todo,

    // Terminators
    @"if": struct { cond: Ref, true: Location, false: Location },
    match: struct {
        value: Ref,
        patterns: std.ArrayList(Ref),
    },
    goto: Location,
    @"return",

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .add => |v| try writer.print("{d} + {d}", .{ v.lhs, v.rhs }),
            .sub => |v| try writer.print("{d} - {d}", .{ v.lhs, v.rhs }),
            .mul => |v| try writer.print("{d} * {d}", .{ v.lhs, v.rhs }),
            .div => |v| try writer.print("{d} / {d}", .{ v.lhs, v.rhs }),

            .cmp_lt => |v| try writer.print("{d} < {d}", .{ v.lhs, v.rhs }),
            .cmp_gt => |v| try writer.print("{d} > {d}", .{ v.lhs, v.rhs }),
            .cmp_le => |v| try writer.print("{d} <= {d}", .{ v.lhs, v.rhs }),
            .cmp_ge => |v| try writer.print("{d} >= {d}", .{ v.lhs, v.rhs }),
            .cmp_eq => |v| try writer.print("{d} == {d}", .{ v.lhs, v.rhs }),
            .cmp_ne => |v| try writer.print("{d} != {d}", .{ v.lhs, v.rhs }),

            .negate => |v| try writer.print("-{d}", .{v.value}),

            .cast => |v| try writer.print("cast({d}, {d})", .{ v.value, v.type }),

            .assign => |v| try writer.print("assign({d})", .{v.value}),

            .load => |v| try writer.print("load({s})", .{v.identifier}),
            .set => |v| try writer.print("set({s}, {d})", .{ v.identifier, v.value }),

            .int => |v| try writer.print("int({s})", .{v}),
            .float => |v| try writer.print("float({s})", .{v}),
            .string => |v| try writer.print("string(\"{s}\")", .{v}),
            .bool => |v| try writer.print("bool({s})", .{if (v) "true" else "false"}),
            .fn_ref => |v| try writer.print("{s}", .{v}),

            .push => |v| try writer.print("push({s})", .{v}),
            .pop => |v| try writer.print("pop({s})", .{v.addr}),

            .create_struct => |_| try writer.print("create_struct", .{}),
            .create_interface => |_| try writer.print("create_interface", .{}),

            .call => |v| try writer.print("call({s})", .{v}),

            .todo => try writer.print("todo", .{}),

            .@"if" => |v| try writer.print("if {s} then {s} else {s}", .{
                v.cond,
                v.true,
                v.false,
            }),
            .match => |v| {
                try writer.print("match {s} ({s})", .{ v.value, v.patterns.items });
            },
            .goto => |v| try writer.print("goto {s}", .{v}),
            .@"return" => try writer.print("return", .{}),
        }
    }
};

pub const FnIndex = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("fn({d})", .{@intFromEnum(self)});
    }
};

pub const Fn = struct {
    const Param = struct {
        ty: Location,
        name: []const u8,
    };

    params: std.ArrayList(Param),
    ret: Location,
    body: Location,

    pub fn deinit(self: *Fn) void {
        self.params.deinit();
        self.ret.deinit();
        self.body.deinit();
    }
};

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

pub const Terminator = union(enum) {
    @"if": struct { cond: Ref, true: Location, false: Location },
    @"switch": struct {
        value: Ref,
        patterns: std.ArrayList(Ref),
    },
    goto: Location,
    @"return",

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .@"if" => |n| try writer.print("if({d}) {d} else {d}", .{
                n.cond,
                n.true,
                n.false,
            }),
            .@"switch" => |n| try writer.print("switch({d}) {{ {s} }}", .{
                n.value,
                n.patterns.items,
            }),

            .goto => |n| try writer.print("goto({d})", .{n}),
            .@"return" => try writer.print("return", .{}),
        }
    }
};

pub const Location = enum(usize) { _ };

pub const Block = struct {
    instructions: std.ArrayList(Inst),
    terminator: ?Terminator = null,

    parent: ?Location = null,

    const Self = @This();

    pub fn make(allocator: Allocator) @This() {
        return @This(){
            .instructions = .init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.instructions.deinit();
        self.terminator = null;
    }

    pub fn append(self: *@This(), inst: Inst) Ref {
        const idx = self.instructions.items.len;
        self.instructions.append(inst) catch |err| {
            @panic(@errorName(err));
        };
        return @enumFromInt(idx);
    }
};

pub const Graph = struct {
    owner: *Self,
    allocator: Allocator,
    blocks: std.ArrayList(Block),
    counter: usize = 0,
    start: Location = @enumFromInt(0),

    pub fn make(owner: *Self, allocator: Allocator, node: AST.NodeRef) Graph {
        var graph = Graph{
            .owner = owner,
            .allocator = allocator,
            .blocks = .init(allocator),
        };

        const block = graph.new_block();
        graph.start = block;

        switch (node.*) {
            .Scope => |lst| {
                for (lst.items) |n| {
                    graph.emitStmt(block, n);
                }
            },
            else => _ = graph.emitExpression(block, node),
        }

        return graph;
    }

    pub fn deinit(self: *Graph) void {
        for (self.blocks.items) |*block| block.deinit();
        self.blocks.deinit();
    }

    pub fn new_block(self: *Graph) Location {
        const idx = self.blocks.items.len;

        self.blocks.append(Block.make(self.allocator)) catch |err| {
            @panic(@errorName(err));
        };

        return @enumFromInt(idx);
    }

    pub fn get(self: *Graph, loc: Location) *Block {
        return &self.blocks.items[@intFromEnum(loc)];
    }

    fn emitExpression(
        self: *Graph,
        loc: Location,
        node: AST.NodeRef,
    ) Ref {
        const block = self.get(loc);

        return switch (node.*) {
            .Unary => |n| node: {
                const value = self.emitExpression(loc, n.node);
                break :node switch (n.Op.ty) {
                    .Minus => block.append(.{ .negate = .{
                        .value = value,
                    } }),
                    else => unreachable,
                };
            },
            .Binary => |n| node: {
                const lhs = self.emitExpression(loc, n.lhs);
                const rhs = self.emitExpression(loc, n.rhs);

                break :node switch (n.Op.ty) {
                    .Plus => block.append(.{ .add = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),
                    .Minus => block.append(.{ .sub = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),
                    .Asterix => block.append(.{ .mul = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),
                    .Slash => block.append(.{ .div = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),

                    .Less => block.append(.{ .cmp_lt = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),
                    .LessEq => block.append(.{ .cmp_le = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),
                    .Greater => block.append(.{ .cmp_gt = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),
                    .GreaterEq => block.append(.{ .cmp_ge = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } }),

                    else => unreachable,
                };
            },
            .String => |v| block.append(.{ .string = v }),
            .Int => |v| block.append(.{ .int = v }),
            .Float => |v| block.append(.{ .float = v }),
            .Bool => |v| block.append(.{ .bool = v }),
            .Ident => |v| node: {
                break :node block.append(.{ .load = .{
                    .identifier = v.text,
                } });
            },
            .FnCall => |n| node: {
                for (n.args.items) |arg| {
                    const ref = self.emitExpression(loc, arg);
                    _ = block.append(.{ .push = ref });
                }

                const @"fn" = self.emitExpression(loc, n.@"fn");
                break :node block.append(.{ .call = @"fn" });
            },
            .FnDecl => |_| node: {
                const ref = self.owner.emit_fn(node);
                break :node block.append(.{ .fn_ref = ref });
            },
            .Struct => block.append(.todo),
            .Comptime => block.append(.todo),
            else => @panic(@tagName(node.*)),
        };
    }

    fn emitStmt(
        self: *Graph,
        loc: Location,
        node: AST.NodeRef,
    ) void {
        const block = self.get(loc);

        switch (node.*) {
            .ConstDecl => |n| {
                const ident = getIdent(n.ident) orelse "Unknown identifier";
                const value = self.emitExpression(loc, n.value);

                _ = block.append(.{ .set = .{
                    .identifier = ident,
                    .value = value,
                } });
            },
            .VarDecl => |n| {
                const ident = getIdent(n.ident) orelse "Unknown identifier";
                const value = self.emitExpression(loc, n.value);

                _ = block.append(.{ .set = .{
                    .identifier = ident,
                    .value = value,
                } });
            },
            .Assignment => |n| {
                const ident = getIdent(n.ident) orelse "Unknown identifier";
                const value = self.emitExpression(loc, n.value);

                _ = block.append(.{ .set = .{
                    .identifier = ident,
                    .value = value,
                } });
            },
            else => {},
        }
    }
};

allocator: Allocator,
functions: std.ArrayList(Fn),
globals: std.StringHashMap(Graph),
scopes: Scopes,
ast: AST,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    return Self{
        .allocator = allocator,
        .functions = .init(allocator),
        .globals = .init(allocator),
        .scopes = .make(allocator),
        .ast = ast,
    };
}

pub fn deinit(self: *Self) void {
    for (self.functions.items) |*func| func.deinit();
    self.functions.deinit();

    var globalIter = self.globals.iterator();
    while (globalIter.next()) |entry| {
        entry.value_ptr.deinit();
    }
    self.globals.deinit();

    self.scopes.deinit();
}

fn emit_fn(self: *Self, node: AST.NodeRef) FnIndex {
    const @"fn": Fn = switch (node.*) {
        .FnDecl => |decl| node: {
            const body = Graph.make(self, self.allocator, decl.block);
            const ret = Graph.make(self, self.allocator, decl.ret);
            const params = lst: {
                var list: std.ArrayList(Fn.Param) = .init(self.allocator);
                for (decl.params.ParamaterList.items) |param| {
                    list.append(.{
                        .ty = Graph.make(self, self.allocator, param.Paramater.type),
                        .name = getIdent(param.Paramater.ident) orelse unreachable,
                    }) catch unreachable;
                }

                break :lst list;
            };

            break :node Fn{
                .params = params,
                .ret = ret,
                .body = body,
            };
        },
        else => unreachable,
    };

    const idx = self.functions.items.len;
    self.functions.append(@"fn") catch |err| {
        @panic(@errorName(err));
    };
    return @enumFromInt(idx);
}

fn getIdent(node: AST.NodeRef) ?[]const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => null,
    };
}

pub fn emitGlobal(self: *Self, node: AST.NodeRef) void {
    switch (node.*) {
        .VarDecl => |n| {
            const ident = getIdent(n.ident) orelse "";
            const graph = Graph.make(self, self.allocator, n.value);
            self.globals.put(ident, graph) catch |err| {
                @panic(@errorName(err));
            };
        },
        .ConstDecl => |n| {
            const ident = getIdent(n.ident) orelse "";
            const graph = Graph.make(self, self.allocator, n.value);
            self.globals.put(ident, graph) catch |err| {
                @panic(@errorName(err));
            };
        },
        else => unreachable,
    }
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                for (lst.items) |n| {
                    self.emitGlobal(n);
                }
            },
            else => unreachable,
        }
    }
}

fn write_ident(
    writer: anytype,
    indent: usize,
    comptime fmt: []const u8,
    args: anytype,
) !void {
    for (0..indent) |_| try writer.print("\t", .{});
    try writer.print(fmt, args);
    try writer.print("\n", .{});
}

fn print_graph(
    self: *Self,
    graph: *Graph,
    indent: usize,
    writer: anytype,
) !void {
    for (graph.blocks.items, 0..) |block, block_idx| {
        try write_ident(writer, indent, "block {d} {{", .{block_idx});

        for (block.instructions.items, 0..) |inst, ref| {
            const instStr = std.fmt.allocPrint(
                self.allocator,
                "{s}",
                .{inst},
            ) catch |err| {
                @panic(@errorName(err));
            };
            defer self.allocator.free(instStr);
            try write_ident(writer, indent + 1, "t%{d} = {s};", .{ ref, instStr });
        }

        if (block.terminator) |terminator| {
            try write_ident(writer, indent + 1, "{s};", .{terminator});
        }

        try write_ident(writer, indent, "}}", .{});
    }
}

pub fn print(self: *Self) void {
    const out = std.debug.print;

    out("### FUNCTIONS ###\n", .{});
    for (self.functions.items, 0..) |@"fn", idx| {
        out("fn {d} {{\n", .{idx});

        out("\tparams = {{\n", .{});
        for (@"fn".params.items) |param| {
            out("\t\t{s}\n", .{param.name}); // TODO: print type
        }
        out("\t}}\n", .{});

        out("\tret = {{\n", .{});
        for (@"fn".ret.blocks.items, 0..) |block, block_idx| {
            out("\t\tblock {d} {{\n", .{block_idx});

            for (block.instructions.items, 0..) |inst, ref| {
                const instStr = std.fmt.allocPrint(
                    self.allocator,
                    "{s}",
                    .{inst},
                ) catch |err| {
                    @panic(@errorName(err));
                };
                defer self.allocator.free(instStr);
                out("\t\t\t%{d} = {s};\n", .{ ref, instStr });
            }

            if (block.terminator) |terminator| {
                out("\t\t\t{s};\n", .{terminator});
            }

            out("\t\t}}\n", .{});
        }

        out("\t}}\n", .{});

        for (@"fn".body.blocks.items, 0..) |block, block_idx| {
            out("\tblock {d} {{\n", .{block_idx});

            for (block.instructions.items, 0..) |inst, ref| {
                const instStr = std.fmt.allocPrint(
                    self.allocator,
                    "{s}",
                    .{inst},
                ) catch |err| {
                    @panic(@errorName(err));
                };
                defer self.allocator.free(instStr);
                out("\t\t%{d} = {s};\n", .{ ref, instStr });
            }

            if (block.terminator) |terminator| {
                out("\t\t{s};\n", .{terminator});
            }

            out("\t}}\n", .{});
        }

        out("}}\n\n", .{});
    }

    out("### GLOBALS ###\n", .{});
    var globalIter = self.globals.iterator();
    while (globalIter.next()) |entry| {
        out("{s} = {{\n", .{entry.key_ptr.*});

        const graph = entry.value_ptr;
        for (graph.blocks.items, 0..) |block, block_idx| {
            out("\tblock {d} {{\n", .{block_idx});

            for (block.instructions.items, 0..) |inst, ref| {
                const instStr = std.fmt.allocPrint(
                    self.allocator,
                    "{s}",
                    .{inst},
                ) catch |err| {
                    @panic(@errorName(err));
                };
                defer self.allocator.free(instStr);
                out("\t\t%{d} = {s};\n", .{ ref, instStr });
            }

            if (block.terminator) |terminator| {
                out("\t\t{s};\n", .{terminator});
            }

            out("\t}}\n", .{});
        }

        out("}}\n\n", .{});
    }
}
