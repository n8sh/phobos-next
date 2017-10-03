/** Various hash functions, including integer ones.
 */
module hashfuns;

import std.traits : isIntegral;

pragma(inline, true)
@safe nothrow:

/** See also: http://forum.dlang.org/post/o1igoc$21ma$1@digitalmars.com
    Doesn't work: integers are returned as is.
 */
size_t typeidHashOf(T)(in T x) @trusted
{
    return typeid(T).getHash(&x); // TODO why not pure @nogc?
}

unittest
{
    auto x = typeidHashOf(cast(int)17);
}

pure @nogc:

/** Dummy-hash for benchmarking performance of HashSet. */
ulong identityHash64Of(T)(in T x)
    if (isIntegral!T &&
        T.sizeof <= ulong.sizeof)
{
    return x;               // maps -1 to ulong.max
}

unittest
{
    assert(identityHash64Of(-1) == ulong.max);
    assert(identityHash64Of(int.max) == int.max);
    assert(identityHash64Of(ulong.max) == ulong.max);
}

/** Mueller integer hash function (bit mixer) A (32-bit).

    See also: https://stackoverflow.com/a/12996028/683710
    See also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
 */
uint muellerHash32(uint x)
{
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

/** Mueller integer hash function (bit mixer) A (64-bit).

    Based on splitmix64, which seems to be based on the blog article "Better Bit
    Mixing" (mix 13).

    See also: https://stackoverflow.com/a/12996028/683710
    See also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
    See also: http://xorshift.di.unimi.it/splitmix64.c
 */
ulong muellerHash64(T)(T x)
    if (isIntegral!T &&
        T.sizeof <= ulong.sizeof)
{
    typeof(return) y = x;
    y = (y ^ (y >> 30)) * 0xbf58476d1ce4e5b9UL;
    y = (y ^ (y >> 27)) * 0x94d049bb133111ebUL;
    y = y ^ (y >> 31);
    return y;
}

/** Thomas Wang 64-bit mix integer hash function.

    See also: https://gist.github.com/badboy/6267743#64-bit-mix-functions
 */
public ulong wangMixHash64(ulong x)
{
    x = (~x) + (x << 21); // x = (x << 21) - x - 1;
    x = x ^ (x >>> 24);
    x = (x + (x << 3)) + (x << 8); // x * 265
    x = x ^ (x >>> 14);
    x = (x + (x << 2)) + (x << 4); // x * 21
    x = x ^ (x >>> 28);
    x = x + (x << 31);
    return x;
}

unittest
{
    assert(wangMixHash64(0) == 8633297058295171728UL);
    assert(wangMixHash64(1) == 6614235796240398542UL);
}
