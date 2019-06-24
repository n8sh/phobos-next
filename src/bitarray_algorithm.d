/** Algorithms for dynamic and static bitarrays based with integral blocks.
 */
module bitarray_algorithm;

import std.traits : isIntegral;

size_t countOnes(Blocks)(const scope auto ref Blocks blocks)
if (isIntegral!(typeof(Blocks.init[0])))
{
    typeof(return) n = 0;
    foreach (const block; blocks)
    {
        import core.bitop : popcnt;
        n += cast(uint)block.popcnt;
    }
    return typeof(return)(n);
}

///
@safe pure nothrow @nogc unittest
{
    enum n = 3;
    size_t[n] x;
    assert(countOnes(x) == 0);

    x[0] = 1;
    assert(countOnes(x) == 1);

    x[0] = 1+2;
    assert(countOnes(x) == 2);

    x[0] = 1+2;
    x[1] = 1+2;
    x[2] = 1+2;
    assert(countOnes(x) == 6);
}
