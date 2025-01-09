const std = @import("../index.zig");
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Token = std.zig.Token;
const mem = std.mem;

pub const Node = struct {
    id: Id,
    comment: ?&NodeLineComment,

    pub const Id = enum {
        Root,
        VarDecl,
        Identifier,
        FnProto,
        ParamDecl,
        Block,
        InfixOp,
        PrefixOp,
        IntegerLiteral,
        FloatLiteral,
        StringLiteral,
        BuiltinCall,
        LineComment,
    };

    pub fn iterate(base: &Node, index: usize) ?&Node {
        return switch (base.id) {
            Id.Root => @fieldParentPtr(NodeRoot, "base", base).iterate(index),
            Id.VarDecl => @fieldParentPtr(NodeVarDecl, "base", base).iterate(index),
            Id.Identifier => @fieldParentPtr(NodeIdentifier, "base", base).iterate(index),
            Id.FnProto => @fieldParentPtr(NodeFnProto, "base", base).iterate(index),
            Id.ParamDecl => @fieldParentPtr(NodeParamDecl, "base", base).iterate(index),
            Id.Block => @fieldParentPtr(NodeBlock, "base", base).iterate(index),
            Id.InfixOp => @fieldParentPtr(NodeInfixOp, "base", base).iterate(index),
            Id.PrefixOp => @fieldParentPtr(NodePrefixOp, "base", base).iterate(index),
            Id.IntegerLiteral => @fieldParentPtr(NodeIntegerLiteral, "base", base).iterate(index),
            Id.FloatLiteral => @fieldParentPtr(NodeFloatLiteral, "base", base).iterate(index),
            Id.StringLiteral => @fieldParentPtr(NodeStringLiteral, "base", base).iterate(index),
            Id.BuiltinCall => @fieldParentPtr(NodeBuiltinCall, "base", base).iterate(index),
            Id.LineComment => @fieldParentPtr(NodeLineComment, "base", base).iterate(index),
        };
    }

    pub fn firstToken(base: &Node) Token {
        return switch (base.id) {
            Id.Root => @fieldParentPtr(NodeRoot, "base", base).firstToken(),
            Id.VarDecl => @fieldParentPtr(NodeVarDecl, "base", base).firstToken(),
            Id.Identifier => @fieldParentPtr(NodeIdentifier, "base", base).firstToken(),
            Id.FnProto => @fieldParentPtr(NodeFnProto, "base", base).firstToken(),
            Id.ParamDecl => @fieldParentPtr(NodeParamDecl, "base", base).firstToken(),
            Id.Block => @fieldParentPtr(NodeBlock, "base", base).firstToken(),
            Id.InfixOp => @fieldParentPtr(NodeInfixOp, "base", base).firstToken(),
            Id.PrefixOp => @fieldParentPtr(NodePrefixOp, "base", base).firstToken(),
            Id.IntegerLiteral => @fieldParentPtr(NodeIntegerLiteral, "base", base).firstToken(),
            Id.FloatLiteral => @fieldParentPtr(NodeFloatLiteral, "base", base).firstToken(),
            Id.StringLiteral => @fieldParentPtr(NodeStringLiteral, "base", base).firstToken(),
            Id.BuiltinCall => @fieldParentPtr(NodeBuiltinCall, "base", base).firstToken(),
            Id.LineComment => @fieldParentPtr(NodeLineComment, "base", base).firstToken(),
        };
    }

    pub fn lastToken(base: &Node) Token {
        return switch (base.id) {
            Id.Root => @fieldParentPtr(NodeRoot, "base", base).lastToken(),
            Id.VarDecl => @fieldParentPtr(NodeVarDecl, "base", base).lastToken(),
            Id.Identifier => @fieldParentPtr(NodeIdentifier, "base", base).lastToken(),
            Id.FnProto => @fieldParentPtr(NodeFnProto, "base", base).lastToken(),
            Id.ParamDecl => @fieldParentPtr(NodeParamDecl, "base", base).lastToken(),
            Id.Block => @fieldParentPtr(NodeBlock, "base", base).lastToken(),
            Id.InfixOp => @fieldParentPtr(NodeInfixOp, "base", base).lastToken(),
            Id.PrefixOp => @fieldParentPtr(NodePrefixOp, "base", base).lastToken(),
            Id.IntegerLiteral => @fieldParentPtr(NodeIntegerLiteral, "base", base).lastToken(),
            Id.FloatLiteral => @fieldParentPtr(NodeFloatLiteral, "base", base).lastToken(),
            Id.StringLiteral => @fieldParentPtr(NodeStringLiteral, "base", base).lastToken(),
            Id.BuiltinCall => @fieldParentPtr(NodeBuiltinCall, "base", base).lastToken(),
            Id.LineComment => @fieldParentPtr(NodeLineComment, "base", base).lastToken(),
        };
    }
};

pub const NodeRoot = struct {
    base: Node,
    decls: ArrayList(&Node),
    eof_token: Token,

    pub fn iterate(self: &NodeRoot, index: usize) ?&Node {
        if (index < self.decls.len) {
            return self.decls.items[self.decls.len - index - 1];
        }
        return null;
    }

    pub fn firstToken(self: &NodeRoot) Token {
        return if (self.decls.len == 0) self.eof_token else self.decls.at(0).firstToken();
    }

    pub fn lastToken(self: &NodeRoot) Token {
        return if (self.decls.len == 0) self.eof_token else self.decls.at(self.decls.len - 1).lastToken();
    }
};

pub const NodeVarDecl = struct {
    base: Node,
    visib_token: ?Token,
    name_token: Token,
    eq_token: Token,
    mut_token: Token,
    comptime_token: ?Token,
    extern_token: ?Token,
    lib_name: ?&Node,
    type_node: ?&Node,
    align_node: ?&Node,
    init_node: ?&Node,
    semicolon_token: Token,

    pub fn iterate(self: &NodeVarDecl, index: usize) ?&Node {
        var i = index;

        if (self.type_node) |type_node| {
            if (i < 1) return type_node;
            i -= 1;
        }

        if (self.align_node) |align_node| {
            if (i < 1) return align_node;
            i -= 1;
        }

        if (self.init_node) |init_node| {
            if (i < 1) return init_node;
            i -= 1;
        }

        return null;
    }

    pub fn firstToken(self: &NodeVarDecl) Token {
        if (self.visib_token) |visib_token| return visib_token;
        if (self.comptime_token) |comptime_token| return comptime_token;
        if (self.extern_token) |extern_token| return extern_token;
        assert(self.lib_name == null);
        return self.mut_token;
    }

    pub fn lastToken(self: &NodeVarDecl) Token {
        return self.semicolon_token;
    }
};

pub const NodeIdentifier = struct {
    base: Node,
    name_token: Token,

    pub fn iterate(self: &NodeIdentifier, index: usize) ?&Node {
        return null;
    }

    pub fn firstToken(self: &NodeIdentifier) Token {
        return self.name_token;
    }

    pub fn lastToken(self: &NodeIdentifier) Token {
        return self.name_token;
    }
};

pub const NodeFnProto = struct {
    base: Node,
    visib_token: ?Token,
    fn_token: Token,
    name_token: ?Token,
    params: ArrayList(&Node),
    return_type: ReturnType,
    var_args_token: ?Token,
    extern_token: ?Token,
    inline_token: ?Token,
    cc_token: ?Token,
    body_node: ?&Node,
    lib_name: ?&Node, // populated if this is an extern declaration
    align_expr: ?&Node, // populated if align(A) is present

    pub const ReturnType = union(enum) {
        Explicit: &Node,
        Infer: Token,
        InferErrorSet: &Node,
    };

    pub fn iterate(self: &NodeFnProto, index: usize) ?&Node {
        var i = index;

        if (self.body_node) |body_node| {
            if (i < 1) return body_node;
            i -= 1;
        }

        switch (self.return_type) {
            // TODO allow this and next prong to share bodies since the types are the same
            ReturnType.Explicit => |node| {
                if (i < 1) return node;
                i -= 1;
            },
            ReturnType.InferErrorSet => |node| {
                if (i < 1) return node;
                i -= 1;
            },
            ReturnType.Infer => {},
        }

        if (self.align_expr) |align_expr| {
            if (i < 1) return align_expr;
            i -= 1;
        }

        if (i < self.params.len) return self.params.items[self.params.len - i - 1];
        i -= self.params.len;

        if (self.lib_name) |lib_name| {
            if (i < 1) return lib_name;
            i -= 1;
        }

        return null;
    }

    pub fn firstToken(self: &NodeFnProto) Token {
        if (self.visib_token) |visib_token| return visib_token;
        if (self.extern_token) |extern_token| return extern_token;
        assert(self.lib_name == null);
        if (self.inline_token) |inline_token| return inline_token;
        if (self.cc_token) |cc_token| return cc_token;
        return self.fn_token;
    }

    pub fn lastToken(self: &NodeFnProto) Token {
        if (self.body_node) |body_node| return body_node.lastToken();
        switch (self.return_type) {
            // TODO allow this and next prong to share bodies since the types are the same
            ReturnType.Explicit => |node| return node.lastToken(),
            ReturnType.InferErrorSet => |node| return node.lastToken(),
            ReturnType.Infer => |token| return token,
        }
    }
};

pub const NodeParamDecl = struct {
    base: Node,
    comptime_token: ?Token,
    noalias_token: ?Token,
    name_token: ?Token,
    type_node: &Node,
    var_args_token: ?Token,

    pub fn iterate(self: &NodeParamDecl, index: usize) ?&Node {
        var i = index;

        if (i < 1) return self.type_node;
        i -= 1;

        return null;
    }

    pub fn firstToken(self: &NodeParamDecl) Token {
        if (self.comptime_token) |comptime_token| return comptime_token;
        if (self.noalias_token) |noalias_token| return noalias_token;
        if (self.name_token) |name_token| return name_token;
        return self.type_node.firstToken();
    }

    pub fn lastToken(self: &NodeParamDecl) Token {
        if (self.var_args_token) |var_args_token| return var_args_token;
        return self.type_node.lastToken();
    }
};

pub const NodeBlock = struct {
    base: Node,
    begin_token: Token,
    end_token: Token,
    statements: ArrayList(&Node),

    pub fn iterate(self: &NodeBlock, index: usize) ?&Node {
        var i = index;

        if (i < self.statements.len) return self.statements.items[i];
        i -= self.statements.len;

        return null;
    }

    pub fn firstToken(self: &NodeBlock) Token {
        return self.begin_token;
    }

    pub fn lastToken(self: &NodeBlock) Token {
        return self.end_token;
    }
};

pub const NodeInfixOp = struct {
    base: Node,
    op_token: Token,
    lhs: &Node,
    op: InfixOp,
    rhs: &Node,

    const InfixOp = enum {
        EqualEqual,
        BangEqual,
        Period,
    };

    pub fn iterate(self: &NodeInfixOp, index: usize) ?&Node {
        var i = index;

        if (i < 1) return self.lhs;
        i -= 1;

        switch (self.op) {
            InfixOp.EqualEqual,
            InfixOp.BangEqual,
            InfixOp.Period => {},
        }

        if (i < 1) return self.rhs;
        i -= 1;

        return null;
    }

    pub fn firstToken(self: &NodeInfixOp) Token {
        return self.lhs.firstToken();
    }

    pub fn lastToken(self: &NodeInfixOp) Token {
        return self.rhs.lastToken();
    }
};

pub const NodePrefixOp = struct {
    base: Node,
    op_token: Token,
    op: PrefixOp,
    rhs: &Node,

    const PrefixOp = union(enum) {
        Return,
        Try,
        AddrOf: AddrOfInfo,
    };
    const AddrOfInfo = struct {
        align_expr: ?&Node,
        bit_offset_start_token: ?Token,
        bit_offset_end_token: ?Token,
        const_token: ?Token,
        volatile_token: ?Token,
    };

    pub fn iterate(self: &NodePrefixOp, index: usize) ?&Node {
        var i = index;

        switch (self.op) {
            PrefixOp.Return,
            PrefixOp.Try => {},
            PrefixOp.AddrOf => |addr_of_info| {
                if (addr_of_info.align_expr) |align_expr| {
                    if (i < 1) return align_expr;
                    i -= 1;
                }
            },
        }

        if (i < 1) return self.rhs;
        i -= 1;

        return null;
    }

    pub fn firstToken(self: &NodePrefixOp) Token {
        return self.op_token;
    }

    pub fn lastToken(self: &NodePrefixOp) Token {
        return self.rhs.lastToken();
    }
};

pub const NodeIntegerLiteral = struct {
    base: Node,
    token: Token,

    pub fn iterate(self: &NodeIntegerLiteral, index: usize) ?&Node {
        return null;
    }

    pub fn firstToken(self: &NodeIntegerLiteral) Token {
        return self.token;
    }

    pub fn lastToken(self: &NodeIntegerLiteral) Token {
        return self.token;
    }
};

pub const NodeFloatLiteral = struct {
    base: Node,
    token: Token,

    pub fn iterate(self: &NodeFloatLiteral, index: usize) ?&Node {
        return null;
    }

    pub fn firstToken(self: &NodeFloatLiteral) Token {
        return self.token;
    }

    pub fn lastToken(self: &NodeFloatLiteral) Token {
        return self.token;
    }
};

pub const NodeBuiltinCall = struct {
    base: Node,
    builtin_token: Token,
    params: ArrayList(&Node),
    rparen_token: Token,

    pub fn iterate(self: &NodeBuiltinCall, index: usize) ?&Node {
        var i = index;

        if (i < self.params.len) return self.params.at(i);
        i -= self.params.len;

        return null;
    }

    pub fn firstToken(self: &NodeBuiltinCall) Token {
        return self.builtin_token;
    }

    pub fn lastToken(self: &NodeBuiltinCall) Token {
        return self.rparen_token;
    }
};

pub const NodeStringLiteral = struct {
    base: Node,
    token: Token,

    pub fn iterate(self: &NodeStringLiteral, index: usize) ?&Node {
        return null;
    }

    pub fn firstToken(self: &NodeStringLiteral) Token {
        return self.token;
    }

    pub fn lastToken(self: &NodeStringLiteral) Token {
        return self.token;
    }
};

pub const NodeLineComment = struct {
    base: Node,
    lines: ArrayList(Token),

    pub fn iterate(self: &NodeLineComment, index: usize) ?&Node {
        return null;
    }

    pub fn firstToken(self: &NodeLineComment) Token {
        return self.lines.at(0);
    }

    pub fn lastToken(self: &NodeLineComment) Token {
        return self.lines.at(self.lines.len - 1);
    }
};
