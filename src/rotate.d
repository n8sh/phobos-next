module rotate;

/** Rotate `x` left by `n` bits.
 *
 * Should compile to a single CPU instruction (ROL).
 */
ulong rotateLeft(ulong x, ubyte n) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return (x << n) | (x >> (64 - n));
}

///
@safe pure nothrow @nogc unittest
{
    assert(rotateLeft(1, 1) == 2);
    assert(rotateLeft(2, 2) == 8);
    assert(rotateLeft(3, 3) == 24);
}

/** Rotate `x` right by `n` bits.
 *
 * Should compile to a single CPU instruction (ROR).
 */
ulong rotateRight(ulong x, ubyte n) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return (x >> n) | (x << (64 - n));
}
