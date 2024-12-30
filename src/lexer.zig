const std = @import("std");
const Token = @import("token.zig");

allocator: std.mem.Allocator,

src: []const u8,

pos: usize,
start: usize,

line: usize,

start_offset: usize,
offset: usize,

const Self = @This();

pub fn make(
    allocator: std.mem.Allocator,
    src: []const u8,
) !Self {
    return Self{
        .allocator = allocator,
        .src = src,

        .pos = 0,
        .start = 0,

        .line = 1,

        .start_offset = 1,
        .offset = 1,
    };
}

pub fn deinit(_: *Self) void {}

pub fn run(self: *Self) std.ArrayList(Token) {
    var out: Token.List = .init(self.allocator);

    while (self.pos < self.src.len) {
        self.start = self.pos;
        self.start_offset = self.offset;

        switch (self.current()) {
            '(' => self.appendSingle(&out, .LParen),
            ')' => self.appendSingle(&out, .RParen),
            '{' => self.appendSingle(&out, .LBrace),
            '}' => self.appendSingle(&out, .RBrace),
            '[' => self.appendSingle(&out, .LBracket),
            ']' => self.appendSingle(&out, .RBracket),

            '.' => self.appendSingle(&out, .Dot),
            ',' => self.appendSingle(&out, .Comma),

            ':' => self.appendSingle(&out, .Colon),
            ';' => self.appendSingle(&out, .Semicolon),

            '+' => self.appendSingle(&out, .Plus),
            '-' => self.appendSingleOrNext(&out, '>', .Arrow, .Minus),
            '*' => self.appendSingle(&out, .Asterix),
            '/' => self.appendSingle(&out, .Minus),

            '>' => self.appendSingleOrNext(&out, '=', .GreaterEq, .Greater),
            '<' => self.appendSingleOrNext(&out, '=', .LessEq, .Less),

            '=' => self.appendSingleOrNext(&out, '=', .EqualEqual, .Equal),
            '!' => self.appendSingleOrNext(&out, '=', .NotEqual, .Not),

            '%' => self.appendSingle(&out, .Percent),

            '"' => {
                self.advance();

                self.start = self.pos;
                self.start_offset = self.offset;

                while (!self.match('"')) : (self.advance()) {}

                self.append(&out, .String);

                self.advance();
            },

            '\n', '\r' => {
                self.advance();

                self.line += 1;
                self.offset = 1;
                self.start_offset = self.offset;
            },

            ' ', '\t' => self.advance(),

            else => {
                if (self.matchFn(identBegin)) {
                    self.advance();

                    while (self.matchFn(identMiddle)) : (self.advance()) {}

                    self.append(&out, .Ident);
                    continue;
                }

                if (self.matchFn(std.ascii.isDigit)) {
                    self.advance();

                    var ty: Token.Ty = .Int;

                    while (true) {
                        if (self.matchFn(std.ascii.isDigit)) {} else if (self.match('.')) {
                            ty = .Float;
                        } else break;

                        self.advance();
                    }

                    //while (self.matchFn(std.ascii.isDigit) or self.match('.')) : (self.advance()) {}

                    self.append(&out, ty);
                    continue;
                }

                std.log.debug("Unknown character: {c}", .{self.current()});
                self.advance();
            },
        }
    }

    return out;
}

pub fn append(self: *Self, out: *Token.List, ty: Token.Ty) void {
    const text = self.src[self.start..self.pos];
    var finalTy = ty;
    if (ty == .Ident) {
        if (Token.Keywords.get(text)) |t| {
            finalTy = t;
        }
    }

    const token = Token{
        .line = self.line,
        .pos = self.start,
        .offset = self.start_offset,

        .text = text,

        .ty = finalTy,
    };

    out.append(token) catch |err| {
        @panic(@errorName(err));
    };
}

pub fn appendSingle(self: *Self, out: *Token.List, ty: Token.Ty) void {
    self.advance();
    self.append(out, ty);
}

pub fn appendSingleOrNext(
    self: *Self,
    out: *Token.List,
    @"if": u8,
    @"true": Token.Ty,
    @"false": Token.Ty,
) void {
    self.advance();

    self.append(out, if (self.advanceIf(@"if")) @"true" else @"false");
}

fn advance(self: *Self) void {
    self.pos += 1;
    self.offset += 1;
}

fn match(self: Self, expected: u8) bool {
    return self.current() == expected;
}

fn matchFn(self: Self, @"fn": *const fn (u8) bool) bool {
    return @"fn"(self.current());
}

fn advanceIf(self: *Self, expected: u8) bool {
    if (self.match(expected)) {
        self.advance();
        return true;
    }

    return false;
}

fn current(self: Self) u8 {
    return self.src[self.pos];
}

fn next(self: Self) u8 {
    return self.src[self.pos + 1];
}

fn EOF(self: Self) bool {
    return self.pos >= self.src.len;
}

fn identBegin(expected: u8) bool {
    return std.ascii.isAlphabetic(expected) or expected == '_';
}

fn identMiddle(expected: u8) bool {
    return identBegin(expected) or std.ascii.isDigit(expected);
}
