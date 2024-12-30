const std = @import("std");

const AST = @import("ast.zig");
const Token = @import("token.zig");

allocator: std.mem.Allocator,

src: Token.List,
pos: usize,
ast: AST,

const Self = @This();

pub fn make(
    allocator: std.mem.Allocator,
    src: Token.List,
) Self {
    return Self{
        .allocator = allocator,

        .src = src,
        .pos = 0,
        .ast = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.ast.deinit();
}

fn alloc(self: *Self, node: AST.Node) AST.NodeRef {
    switch (node) {
        .Error => |err| {
            std.debug.panic("Token: \"{s}\", err -> {s}", .{ err.token.text, err.msg });
        },
        else => {},
    }

    return self.ast.alloc(node);
}

fn eof(self: Self) bool {
    return self.pos >= self.src.items.len;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn getCurrent(self: *Self) Token {
    return self.src.items[self.pos];
}

fn match(self: *Self, expected: Token.Ty) bool {
    return self.getCurrent().ty == expected;
}

fn advanceIf(self: *Self, expected: Token.Ty) ?Token {
    if (self.match(expected)) {
        const tok = self.getCurrent();
        self.advance();
        return tok;
    }

    return null;
}

fn parse_ident(self: *Self) AST.NodeRef {
    const tok = self.getCurrent();

    return if (tok.ty == .Ident) node: {
        self.advance();
        break :node self.alloc(.{ .Ident = tok });
    } else self.alloc(.{ .Error = .{
        .msg = "Unable to parse ident",
        .token = tok,
    } });
}

fn parse_fnDecl(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    return self.alloc(.{ .Error = .{
        .msg = "Not Implemented",
        .token = start,
    } });
}

fn parse_value(self: *Self) AST.NodeRef {
    const tok = self.getCurrent();
    self.advance();

    return switch (tok.ty) {
        .String => self.alloc(.{ .String = tok.text }),
        .Float => self.alloc(.{ .Float = tok.text }),
        .Int => self.alloc(.{ .Int = tok.text }),
        .True => self.alloc(.{ .Bool = true }),
        .False => self.alloc(.{ .Bool = false }),
        .Ident => self.alloc(.{ .Ident = tok }),
        .Fn => self.parse_fnDecl(),
        .LParen => node: {
            self.advance();
            const expr = self.parse_expr();
            if (self.advanceIf(.RParen)) |_| {
                break :node expr;
            }

            break :node self.alloc(.{ .Error = .{
                .msg = "'(' Needs a closing ')'",
                .token = tok,
            } });
        },
        else => self.alloc(.{ .Error = .{
            .msg = "Unable to parse value",
            .token = tok,
        } }),
    };
}

fn expression(
    self: *Self,
    lhs: AST.NodeRef,
    min_precedence: i32,
) AST.NodeRef {
    var final = lhs;
    var current = self.getCurrent();

    while (Token.precendence(current) >= min_precedence) {
        const op = current;
        self.advance();

        var rhs = self.parse_value();

        current = self.getCurrent();

        while (Token.precendence(current) > Token.precendence(op)) {
            rhs = self.expression(
                rhs,
                Token.precendence(op) + @as(i32, if (Token.precendence(current) > Token.precendence(op))
                    1
                else
                    0),
            );
        }

        final = self.alloc(.{ .Binary = .{
            .lhs = final,
            .Op = op,
            .rhs = rhs,
        } });
    }
    return final;
}

fn parse_expr(self: *Self) AST.NodeRef {
    return self.expression(self.parse_value(), 0);
}

fn parse_if(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

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

fn parse_const(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Const)) |_| {
        const ident = self.parse_ident();
        const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_|
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

fn parse_var(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Var)) |_| {
        const ident = self.parse_ident();
        const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_| ty: {
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

fn parse_stmt(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    const stmt = switch (start.ty) {
        .Const => self.parse_const(),
        .Var => self.parse_var(),
        else => self.parse_value(),
    };

    if (self.advanceIf(.Semicolon)) |_| {
        return stmt;
    }

    return self.alloc(.{ .Error = .{
        .msg = "Statement must end with a semicolon",
        .token = start,
    } });
}

fn parse_toplevel(self: *Self) AST.NodeRef {
    var list = std.ArrayList(AST.NodeRef).init(self.allocator);
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
