const __udivmodti4 = @import("udivmodti4.zig").__udivmodti4;
const builtin = @import("builtin");

pub extern fn __udivti3(a: u128, b: u128) u128 {
    @setRuntimeSafety(builtin.is_test);
    return __udivmodti4(a, b, null);
}
