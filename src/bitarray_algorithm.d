/** Algorithms for dynamic and static bitarrays based with integral blocks.
 */
module bitarray_algorithm;

size_t countOnes(Blocks)(const scope auto ref Blocks blocks)
if (is(typeof(Blocks.init[0]) : uint) ||
    is(typeof(Blocks.init[0]) : ulong))
{
    typeof(return) n = 0;
    foreach (const block; blocks)
    {
        import core.bitop : popcnt;
        n += cast(typeof(n))popcnt(block);
    }
    return typeof(return)(n);
}

/// Test `countOnes`.
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
