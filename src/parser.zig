const std = @import("std");

const AST = @import("ast.zig");
const Token = @import("token.zig");
const Log = @import("log.zig");

src: Token.List,
pos: usize,
ast: AST,

const Self = @This();

pub fn make(
    parent_allocator: std.mem.Allocator,
    src: Token.List,
) Self {
    return Self{
        .src = src,
        .pos = 0,
        .ast = .make(parent_allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.ast.deinit();
}

fn allocator(self: *Self) std.mem.Allocator {
    return self.ast.arena.allocator();
}

fn alloc(self: *Self, node: AST.Node) *AST.Node {
    return self.ast.alloc(node);
}

fn eof(self: Self) bool {
    return self.pos >= self.src.items.len;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn current(self: *Self) Token {
    return self.src.items[self.pos];
}

fn match(self: *Self, expected: Token.Ty) bool {
    return self.current().ty == expected;
}

fn advanceIf(self: *Self, expected: Token.Ty) ?Token {
    Log.info("Expected: {s}", .{@tagName(expected)});
    if (self.match(expected)) {
        const tok = self.current();
        self.advance();
        return tok;
    }

    return null;
}

fn parse_ident(self: *Self) *AST.Node {
    const tok = self.current();
    self.advance();
    return self.alloc(.{ .Ident = tok });
}

fn parse_value(self: *Self) *AST.Node {
    const tok = self.current();
    self.advance();

    return switch (tok.ty) {
        .String => self.alloc(.{ .String = tok.text }),
        .Float => self.alloc(.{ .Float = tok.text }),
        .Int => self.alloc(.{ .Int = tok.text }),
        .True => self.alloc(.{ .Bool = true }),
        .False => self.alloc(.{ .Bool = false }),
        .Ident => self.alloc(.{ .Ident = tok }),
        else => self.alloc(.{ .Error = .{
            .msg = "Unable to parse value",
            .token = tok,
        } }),
    };
}

fn parse_expr(self: *Self) *AST.Node {
    Log.info("expr", .{});

    return self.parse_value();
}

fn parse_if(self: *Self) *AST.Node {
    Log.info("if", .{});

    const start = self.current();

    if (self.advanceIf(.If)) |_| {
        const cond = self.parse_expr();
        if (self.advanceIf(.Then)) |_| {
            const @"true" = self.parse_expr();
            if (self.advanceIf(.Else)) |_| {
                const @"false" = self.parse_expr();

                return self.alloc(.{ .If = .{
                    .cond = cond,
                    .false = @"false",
                    .true = @"true",
                } });
            }
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse const statement",
        .token = start,
    } });
}

fn parse_const(self: *Self) *AST.Node {
    Log.info("const", .{});

    const start = self.current();

    if (self.advanceIf(.Const)) |_| {
        const ident = self.parse_ident();
        const @"type": ?*AST.Node = if (self.advanceIf(.Colon)) |_|
            self.parse_expr()
        else
            null;

        if (self.advanceIf(.Equal)) |_| {
            const value = self.parse_expr();

            return self.alloc(.{ .ConstDecl = .{
                .ident = ident,
                .type = @"type",
                .value = value,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse const statement",
        .token = start,
    } });
}

fn parse_var(self: *Self) *AST.Node {
    Log.info("var", .{});

    const start = self.current();

    if (self.advanceIf(.Var)) |_| {
        const ident = self.parse_ident();
        const @"type": ?*AST.Node = if (self.advanceIf(.Colon)) |_| ty: {
            break :ty self.parse_expr();
        } else null;

        if (self.advanceIf(.Equal)) |_| {
            const value = self.parse_expr();

            return self.alloc(.{ .VarDecl = .{
                .ident = ident,
                .type = @"type",
                .value = value,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse var statement",
        .token = start,
    } });
}

fn parse_stmt(self: *Self) *AST.Node {
    const start = self.current();

    Log.info("Statement: {s}", .{start.text});

    return switch (start.ty) {
        .Const => self.parse_const(),
        .Var => self.parse_var(),
        else => node: {
            self.advance();
            break :node self.alloc(.{ .Error = .{
                .msg = "Failed to parse statement",
                .token = start,
            } });
        },
    };
}

fn parse_toplevel(self: *Self) *AST.Node {
    var list = std.ArrayList(*AST.Node).init(self.allocator());
    while (!self.eof()) {
        list.append(self.parse_stmt()) catch |err| {
            @panic(@errorName(err));
        };
    }

    return self.alloc(.{ .Scope = list });
}

pub fn run(self: *Self) void {
    self.ast.root = self.parse_toplevel();
}
