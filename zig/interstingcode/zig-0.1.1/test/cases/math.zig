const assert = @import("std").debug.assert;

test "division" {
    testDivision();
    comptime testDivision();
}
fn testDivision() {
    assert(div(u32, 13, 3) == 4);
    assert(div(f32, 1.0, 2.0) == 0.5);

    assert(divExact(u32, 55, 11) == 5);
    assert(divExact(i32, -55, 11) == -5);
    assert(divExact(f32, 55.0, 11.0) == 5.0);
    assert(divExact(f32, -55.0, 11.0) == -5.0);

    assert(divFloor(i32, 5, 3) == 1);
    assert(divFloor(i32, -5, 3) == -2);
    assert(divFloor(f32, 5.0, 3.0) == 1.0);
    assert(divFloor(f32, -5.0, 3.0) == -2.0);
    assert(divFloor(i32, -0x80000000, -2) == 0x40000000);
    assert(divFloor(i32, 0, -0x80000000) == 0);
    assert(divFloor(i32, -0x40000001, 0x40000000) == -2);
    assert(divFloor(i32, -0x80000000, 1) == -0x80000000);

    assert(divTrunc(i32, 5, 3) == 1);
    assert(divTrunc(i32, -5, 3) == -1);
    assert(divTrunc(f32, 5.0, 3.0) == 1.0);
    assert(divTrunc(f32, -5.0, 3.0) == -1.0);
}
fn div(comptime T: type, a: T, b: T) -> T {
    a / b
}
fn divExact(comptime T: type, a: T, b: T) -> T {
    @divExact(a, b)
}
fn divFloor(comptime T: type, a: T, b: T) -> T {
    @divFloor(a, b)
}
fn divTrunc(comptime T: type, a: T, b: T) -> T {
    @divTrunc(a, b)
}

test "@addWithOverflow" {
    var result: u8 = undefined;
    assert(@addWithOverflow(u8, 250, 100, &result));
    assert(!@addWithOverflow(u8, 100, 150, &result));
    assert(result == 250);
}

// TODO test mulWithOverflow
// TODO test subWithOverflow

test "@shlWithOverflow" {
    var result: u16 = undefined;
    assert(@shlWithOverflow(u16, 0b0010111111111111, 3, &result));
    assert(!@shlWithOverflow(u16, 0b0010111111111111, 2, &result));
    assert(result == 0b1011111111111100);
}

test "@clz" {
    testClz();
    comptime testClz();
}

fn testClz() {
    assert(clz(u8(0b00001010)) == 4);
    assert(clz(u8(0b10001010)) == 0);
    assert(clz(u8(0b00000000)) == 8);
    assert(clz(u128(0xffffffffffffffff)) == 64);
    assert(clz(u128(0x10000000000000000)) == 63);
}

fn clz(x: var) -> usize {
    @clz(x)
}

test "@ctz" {
    testCtz();
    comptime testCtz();
}

fn testCtz() {
    assert(ctz(u8(0b10100000)) == 5);
    assert(ctz(u8(0b10001010)) == 1);
    assert(ctz(u8(0b00000000)) == 8);
}

fn ctz(x: var) -> usize {
    @ctz(x)
}

test "assignment operators" {
    var i: u32 = 0;
    i += 5;  assert(i == 5);
    i -= 2;  assert(i == 3);
    i *= 20; assert(i == 60);
    i /= 3;  assert(i == 20);
    i %= 11; assert(i == 9);
    i <<= 1; assert(i == 18);
    i >>= 2; assert(i == 4);
    i = 6;
    i &= 5;  assert(i == 4);
    i ^= 6;  assert(i == 2);
    i = 6;
    i |= 3;  assert(i == 7);
}

test "three expr in a row" {
    testThreeExprInARow(false, true);
    comptime testThreeExprInARow(false, true);
}
fn testThreeExprInARow(f: bool, t: bool) {
    assertFalse(f or f or f);
    assertFalse(t and t and f);
    assertFalse(1 | 2 | 4 != 7);
    assertFalse(3 ^ 6 ^ 8 != 13);
    assertFalse(7 & 14 & 28 != 4);
    assertFalse(9  << 1 << 2 != 9  << 3);
    assertFalse(90 >> 1 >> 2 != 90 >> 3);
    assertFalse(100 - 1 + 1000 != 1099);
    assertFalse(5 * 4 / 2 % 3 != 1);
    assertFalse(i32(i32(5)) != 5);
    assertFalse(!!false);
    assertFalse(i32(7) != --(i32(7)));
}
fn assertFalse(b: bool) {
    assert(!b);
}


test "const number literal" {
    const one = 1;
    const eleven = ten + one;

    assert(eleven == 11);
}
const ten = 10;



test "unsigned wrapping" {
    testUnsignedWrappingEval(@maxValue(u32));
    comptime testUnsignedWrappingEval(@maxValue(u32));
}
fn testUnsignedWrappingEval(x: u32) {
    const zero = x +% 1;
    assert(zero == 0);
    const orig = zero -% 1;
    assert(orig == @maxValue(u32));
}

test "signed wrapping" {
    testSignedWrappingEval(@maxValue(i32));
    comptime testSignedWrappingEval(@maxValue(i32));
}
fn testSignedWrappingEval(x: i32) {
    const min_val = x +% 1;
    assert(min_val == @minValue(i32));
    const max_val = min_val -% 1;
    assert(max_val == @maxValue(i32));
}

test "negation wrapping" {
    testNegationWrappingEval(@minValue(i16));
    comptime testNegationWrappingEval(@minValue(i16));
}
fn testNegationWrappingEval(x: i16) {
    assert(x == -32768);
    const neg = -%x;
    assert(neg == -32768);
}

test "unsigned 64-bit division" {
    test_u64_div();
    comptime test_u64_div();
}
fn test_u64_div() {
    const result = divWithResult(1152921504606846976, 34359738365);
    assert(result.quotient == 33554432);
    assert(result.remainder == 100663296);
}
fn divWithResult(a: u64, b: u64) -> DivResult {
    DivResult {
        .quotient = a / b,
        .remainder = a % b,
    }
}
const DivResult = struct {
    quotient: u64,
    remainder: u64,
};

test "binary not" {
    assert(comptime {~u16(0b1010101010101010) == 0b0101010101010101});
    assert(comptime {~u64(2147483647) == 18446744071562067968});
    testBinaryNot(0b1010101010101010);
}

fn testBinaryNot(x: u16) {
    assert(~x == 0b0101010101010101);
}

test "small int addition" {
    var x: @IntType(false, 2) = 0;
    assert(x == 0);

    x += 1;
    assert(x == 1);

    x += 1;
    assert(x == 2);

    x += 1;
    assert(x == 3);

    var result: @typeOf(x) = 3;
    assert(@addWithOverflow(@typeOf(x), x, 1, &result));

    assert(result == 0);
}

test "float equality" {
    const x: f64 = 0.012;
    const y: f64 = x + 1.0;

    testFloatEqualityImpl(x, y);
    comptime testFloatEqualityImpl(x, y);
}

fn testFloatEqualityImpl(x: f64, y: f64) {
    const y2 = x + 1.0;
    assert(y == y2);
}

test "allow signed integer division/remainder when values are comptime known and positive or exact" {
    assert(5 / 3 == 1);
    assert(-5 / -3 == 1);
    assert(-6 / 3 == -2);

    assert(5 % 3 == 2);
    assert(-6 % 3 == 0);
}

test "hex float literal parsing" {
    comptime assert(0x1.0 == 1.0);
}

test "quad hex float literal parsing in range" {
    const a = 0x1.af23456789bbaaab347645365cdep+5;
    const b = 0x1.dedafcff354b6ae9758763545432p-9;
    const c = 0x1.2f34dd5f437e849b4baab754cdefp+4534;
    const d = 0x1.edcbff8ad76ab5bf46463233214fp-435;
}

test "hex float literal within range" {
    const a = 0x1.0p16383;
    const b = 0x0.1p16387;
    const c = 0x1.0p-16382;
}

test "truncating shift left" {
    testShlTrunc(@maxValue(u16));
    comptime testShlTrunc(@maxValue(u16));
}
fn testShlTrunc(x: u16) {
    const shifted = x << 1;
    assert(shifted == 65534);
}

test "truncating shift right" {
    testShrTrunc(@maxValue(u16));
    comptime testShrTrunc(@maxValue(u16));
}
fn testShrTrunc(x: u16) {
    const shifted = x >> 1;
    assert(shifted == 32767);
}

test "exact shift left" {
    testShlExact(0b00110101);
    comptime testShlExact(0b00110101);
}
fn testShlExact(x: u8) {
    const shifted = @shlExact(x, 2);
    assert(shifted == 0b11010100);
}

test "exact shift right" {
    testShrExact(0b10110100);
    comptime testShrExact(0b10110100);
}
fn testShrExact(x: u8) {
    const shifted = @shrExact(x, 2);
    assert(shifted == 0b00101101);
}

test "big number addition" {
    comptime {
        assert(
            35361831660712422535336160538497375248 +
            101752735581729509668353361206450473702 ==
            137114567242441932203689521744947848950);
        assert(
            594491908217841670578297176641415611445982232488944558774612 +
            390603545391089362063884922208143568023166603618446395589768 ==
            985095453608931032642182098849559179469148836107390954364380);
    }
}

test "big number multiplication" {
    comptime {
        assert(
            45960427431263824329884196484953148229 *
            128339149605334697009938835852565949723 ==
            5898522172026096622534201617172456926982464453350084962781392314016180490567);
        assert(
            594491908217841670578297176641415611445982232488944558774612 *
            390603545391089362063884922208143568023166603618446395589768 ==
            232210647056203049913662402532976186578842425262306016094292237500303028346593132411865381225871291702600263463125370016);
    }
}

test "big number shifting" {
    comptime {
        assert((u128(1) << 127) == 0x80000000000000000000000000000000);
    }
}

test "f128" {
    test_f128();
    comptime test_f128();
}

fn make_f128(x: f128) -> f128 { x }

fn test_f128() {
    assert(@sizeOf(f128) == 16);
    assert(make_f128(1.0) == 1.0);
    assert(make_f128(1.0) != 1.1);
    assert(make_f128(1.0) > 0.9);
    assert(make_f128(1.0) >= 0.9);
    assert(make_f128(1.0) >= 1.0);
    should_not_be_zero(1.0);
}

fn should_not_be_zero(x: f128) {
    assert(x != 0.0);
}