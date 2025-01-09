const udivmod = @import("udivmod.zig").udivmod;
const builtin = @import("builtin");

pub extern fn __udivmodti4(a: u128, b: u128, maybe_rem: ?&u128) u128 {
    @setRuntimeSafety(builtin.is_test);
    return udivmod(u128, a, b, maybe_rem);
}

test "import udivmodti4" {
    _ = @import("udivmodti4_test.zig");
}
