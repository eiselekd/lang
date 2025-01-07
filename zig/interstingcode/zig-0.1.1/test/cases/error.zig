const assert = @import("std").debug.assert;
const mem = @import("std").mem;

pub fn foo() -> %i32 {
    const x = %return bar();
    return x + 1
}

pub fn bar() -> %i32 {
    return 13;
}

pub fn baz() -> %i32 {
    const y = foo() %% 1234;
    return y + 1;
}

test "error wrapping" {
    assert(%%baz() == 15);
}

error ItBroke;
fn gimmeItBroke() -> []const u8 {
    @errorName(error.ItBroke)
}

test "@errorName" {
    assert(mem.eql(u8, @errorName(error.AnError), "AnError"));
    assert(mem.eql(u8, @errorName(error.ALongerErrorName), "ALongerErrorName"));
}
error AnError;
error ALongerErrorName;


test "error values" {
    const a = i32(error.err1);
    const b = i32(error.err2);
    assert(a != b);
}
error err1;
error err2;


test "redefinition of error values allowed" {
    shouldBeNotEqual(error.AnError, error.SecondError);
}
error AnError;
error AnError;
error SecondError;
fn shouldBeNotEqual(a: error, b: error) {
    if (a == b) unreachable
}


test "error binary operator" {
    const a = errBinaryOperatorG(true) %% 3;
    const b = errBinaryOperatorG(false) %% 3;
    assert(a == 3);
    assert(b == 10);
}
error ItBroke;
fn errBinaryOperatorG(x: bool) -> %isize {
    if (x) {
        error.ItBroke
    } else {
        isize(10)
    }
}


test "unwrap simple value from error" {
    const i = %%unwrapSimpleValueFromErrorDo();
    assert(i == 13);
}
fn unwrapSimpleValueFromErrorDo() -> %isize { 13 }


test "error return in assignment" {
    %%doErrReturnInAssignment();
}

fn doErrReturnInAssignment() -> %void {
    var x : i32 = undefined;
    x = %return makeANonErr();
}

fn makeANonErr() -> %i32 {
    return 1;
}
