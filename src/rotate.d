module rotate;

/** Rotate `x` by `n`.
 *
 * Should compile to a single CPU instruction (ROL).
 */
ulong rotateLeft(ulong x, ubyte n) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return (x << n) | (x >> (64 - n));
}
