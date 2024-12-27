const std = @import("std");

pub const Value = union(enum) {
    Int: i32,
    Float: f32,
    String: []const u8,
    Bool: bool,
};

pub const Inst = union(enum) {
    Jump: usize,

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
};
