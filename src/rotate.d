module bitwise_rotate;

/** Rotate `x` left by `n` bits.
 *
 * Should compile to a single CPU instruction (ROL).
 */
ulong rotateLeft(ulong x, uint n) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return (x << n) | (x >> (8*typeof(x).sizeof - n));
}

///
@safe pure nothrow @nogc unittest
{
    assert(rotateLeft(ulong.max, 1) == ulong.max);
    assert(rotateLeft(1UL, 1) == 2);
    assert(rotateLeft(2UL, 2) == 8);
    assert(rotateLeft(3UL, 3) == 24);
    assert(rotateLeft(2UL^^63, 1) == 1);
}

/** Rotate `x` right by `n` bits.
 *
 * Should compile to a single CPU instruction (ROR).
 */
ulong rotateRight(ulong x, uint n) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return (x >> n) | (x << (8*typeof(x).sizeof - n));
}

///
@safe pure nothrow @nogc unittest
{
    assert(rotateRight(ulong.max, 1) == ulong.max);
    assert(rotateRight(1UL, 1) == 2UL^^63);
}
