module hashfuns;

import std.traits : isIntegral, isUnsigned;

pragma(inline, true):

/** Dummy-hash for benchmarking performance of HashSet. */
ulong identityHashOf(T)(in T value)
    if (isUnsigned!T &&
        T.sizeof <= ulong.sizeof)
{
    return value;
}

/** See also: http://forum.dlang.org/post/o1igoc$21ma$1@digitalmars.com
    Doesn't work: integers are returned as is.
 */
size_t typeidHashOf(T)(in T value) @trusted
{
    return typeid(T).getHash(&value);
}

/** Mueller hash function (bit mixer) A (32-bit).

    See also: https://stackoverflow.com/a/12996028/683710
    See also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
 */
uint muellerHash32(uint x)
    @safe pure nothrow @nogc
{
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

/** Mueller hash function (bit mixer) A (64-bit).

    Based on splitmix64, which seems to be based on the blog article "Better Bit
    Mixing" (mix 13).

    See also: https://stackoverflow.com/a/12996028/683710
    See also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
    See also: http://xorshift.di.unimi.it/splitmix64.c
 */
ulong muellerHash64(T)(T x)
    @safe pure nothrow @nogc
    if (isIntegral!T &&
        T.sizeof <= ulong.sizeof)
{
    typeof(return) y = x;
    y = (y ^ (y >> 30)) * 0xbf58476d1ce4e5b9UL;
    y = (y ^ (y >> 27)) * 0x94d049bb133111ebUL;
    y = y ^ (y >> 31);
    return y;
}
