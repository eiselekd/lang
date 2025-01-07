const assert = @import("std").debug.assert;

test "@bitCast i32 -> u32" {
    testBitCast_i32_u32();
    comptime testBitCast_i32_u32();
}

fn testBitCast_i32_u32() {
    assert(conv(-1) == @maxValue(u32));
    assert(conv2(@maxValue(u32)) == -1);
}

fn conv(x: i32) -> u32 { @bitCast(u32, x) }
fn conv2(x: u32) -> i32 { @bitCast(i32, x) }
