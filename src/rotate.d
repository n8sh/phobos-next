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

/** Rotate `x` right by `n` bits.
 *
 * Should compile to a single CPU instruction (ROR).
 */
ulong rotateRight(ulong x, ubyte n) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return (x >> n) | (x << (64 - n));
}
