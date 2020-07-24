/** Various hash functions, including integer ones.
 */
module nxt.hash_functions;

import std.traits : isIntegral;

pragma(inline, true)
@safe nothrow:

/** See_Also: http://forum.dlang.org/post/o1igoc$21ma$1@digitalmars.com
 *
 * Doesn't work: integers are returned as is.
 */
size_t typeidHashOf(T)(in T x) @trusted
{
    return typeid(T).getHash(&x); // TODO why not pure @nogc?
}

///
unittest
{
    // TODO auto x = typeidHashOf(cast(int)17);
}

hash_t hashOfTypeInfoPtr(TypeInfo_Class typeinfo) @trusted pure nothrow @nogc
{
    assert(typeof(typeinfo).alignof == 8);
    return (cast(hash_t)(cast(void*)typeinfo) >> 3);
}

size_t fibonacci_hash(hash_t hash) @safe pure nothrow @nogc
{
    return (hash * 11400714819323198485LU);
}

/** Hash that incorporates the hash of `typeid` bit-xored with `hashOf(a)`.
 *
 * See_Also: https://forum.dlang.org/post/lxqoknwuujbymolnlyfw@forum.dlang.org
 */
hash_t hashOfPolymorphic(Class)(Class a) @trusted pure nothrow @nogc
if (is(Class == class))
{
    static assert(typeid(Class).alignof == 8);
    // const class_typeid_hash = (cast(hash_t)(cast(void*)typeid(Class)) >> 3)
    import core.internal.hash : hashOf;
    return fibonacci_hash(hashOf(cast(void*)typeid(a))) ^ hashOf(a);
}

version(unittest)
{
    private static:

    class Thing
    {
    }

    class Expr : Thing
    {
        @safe pure nothrow @nogc:
        alias Data = string;
        this(Data data)
        {
            this.data = data;
        }
        @property override hash_t toHash() const @safe pure nothrow @nogc
        {
            return hashOf(data);
        }
        Data data;
    }

    class NounExpr : Expr
    {
        @safe pure nothrow @nogc:
        this(Data data)
        {
            super(data);
        }
        @property override hash_t toHash() const @safe pure nothrow @nogc
        {
            return hashOf(data);
        }
    }

    class Year : Thing
    {
        @safe pure nothrow @nogc:
        alias Data = long;
        @property override hash_t toHash() const @safe pure nothrow @nogc
        {
            return hashOf(data);
        }
        Data data;
    }
}

///
@safe pure nothrow unittest
{
    auto car1 = new Expr("car");
    auto car2 = new Expr("car");
    auto bar1 = new Expr("bar");
    auto ncar = new NounExpr("car");

    void testEqual() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) == hashOf(car2));
        assert(hashOfPolymorphic(car1) == hashOfPolymorphic(car2));
    }

    void testDifferent1() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) != hashOf(bar1));
        assert(hashOfPolymorphic(car1) != hashOfPolymorphic(bar1));
    }

    void testDifferent2() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) == hashOf(ncar));
        assert(hashOfPolymorphic(car1) != hashOfPolymorphic(ncar));
    }

    testEqual();
    testDifferent1();
    testDifferent2();
}

pure @nogc:

/** Dummy-hash for benchmarking performance of HashSet. */
ulong identityHash64Of(T)(in T x)
if (isIntegral!T &&
    T.sizeof <= ulong.sizeof)
{
    return x;               // maps -1 to ulong.max
}

///
unittest
{
    assert(identityHash64Of(-1) == ulong.max);
    assert(identityHash64Of(int.max) == int.max);
    assert(identityHash64Of(ulong.max) == ulong.max);
}

/** Mueller integer hash function (bit mixer) A (32-bit).
 *
 * See_Also: https://stackoverflow.com/a/12996028/683710
 * See_Also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
 */
uint muellerHash32(uint x)
{
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

/** Mueller integer hash function (bit mixer) A (64-bit).
 *
 * Based on splitmix64, which seems to be based on the blog article "Better Bit
 * Mixing" (mix 13).
 *
 * See_Also: https://stackoverflow.com/a/12996028/683710
 * See_Also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
 * See_Also: http://xorshift.di.unimi.it/splitmix64.c
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
 *
 * See_Also: https://gist.github.com/badboy/6267743#64-bit-mix-functions
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

///
unittest
{
    assert(wangMixHash64(0) == 8633297058295171728UL);
    assert(wangMixHash64(1) == 6614235796240398542UL);
}

/** Inspired by lemire's strongly universal hashing.
 *
 * See_Also: https://lemire.me/blog/2018/08/15/fast-strongly-universal-64-bit-hashing-everywhere/
 *
 * Instead of shifts, we use rotations so we don't lose any bits.
 *
 * Added a final multiplcation with a constant for more mixing. It is most important that the
 * lower bits are well mixed.
 */
size_t lemireHash64(in ulong x)
{
    pragma(inline, true);
    import core.bitop : ror;
    const ulong h1 = x * 0xA24BAED4963EE407UL;
    const ulong h2 = ror(x, 32U) * 0x9FB21C651E98DF25UL;
    const ulong h = ror(h1 + h2, 32U);
    return h;
}

///
unittest
{
    assert(lemireHash64(0) == 0UL);
    assert(lemireHash64(1) == 10826341276197359097UL);
    assert(lemireHash64(2) == 3205938474390199283UL);
}
