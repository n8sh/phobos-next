/** Algorithms for dynamic and static bitarrays based with integral blocks.
 */
module bitarray_algorithm;

import std.traits : isArray;

size_t countOnes(Blocks)(const scope auto ref Blocks blocks)
if (is(typeof(Blocks.init[0]) == size_t))
{
    typeof(return) n = 0;
    foreach (const block; blocks)
    {
        import core.bitop : popcnt;
        static if (block.sizeof == 1 ||
                   block.sizeof == 2 ||
                   block.sizeof == 4 ||
                   block.sizeof == 4)
        {
            // TODO do we need to force `uint`-overload of `popcnt`?
            n += cast(uint)block.popcnt;
        }
        else static if (block.sizeof == 8)
        {
            n += (cast(ulong)((cast(uint)(block)).popcnt) +
                  cast(ulong)((cast(uint)(block >> 32)).popcnt));
        }
        else
        {
            assert(0, "Unsupported Block size " ~ Block.sizeof.stringof);
        }
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
