// Untyped IR

const std = @import("std");

instructions: std.ArrayList(Inst).Slice,

const Ref = usize;

const Inst = union(enum) {
    Add: struct { lhs: Ref, rhs: Ref },
    Sub: struct { lhs: Ref, rhs: Ref },
    Div: struct { lhs: Ref, rhs: Ref },
    Mul: struct { lhs: Ref, rhs: Ref },

    Less: struct { lhs: Ref, rhs: Ref },
    Greater: struct { lhs: Ref, rhs: Ref },

    LessEq: struct { lhs: Ref, rhs: Ref },
    GreaterEq: struct { lhs: Ref, rhs: Ref },

    Equal: struct { lhs: Ref, rhs: Ref },
    NotEqual: struct { lhs: Ref, rhs: Ref },

    Negate: struct { value: Ref },

    StartBlock: struct { len: usize },
    ReturnBlock: struct {},
};
