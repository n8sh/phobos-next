module hash_ex;

import std.traits : isScalarType, isAggregateType, hasIndirections, isSomeString, isArray, isPointer;

// TODO make inlining work for LDC
version(LDC)
{
pragma(inline, true):           // TODO make this work
}
pragma(inline):                 // LDC can inline, DMD cannot

/** Digest `value` into `digest`.
 */
void digestAny(Digest, T)(ref Digest digest,
                          in auto ref T value)
{
    // TODO use:
    // static if (hasMember!(hasher, "putStaticArray"))
    // {
    //     digest.putStaticArray((cast(ubyte*)&value)[0 .. value.sizeof]);
    // }
    // else
    // {
    //     digest.put((cast(ubyte*)&value)[0 .. value.sizeof]);
    // }

    static if (isScalarType!T)
    {
        digestRaw(digest, value);
    }
    else static if (is(T == class) && // a class is memory-wise
                    isPointer!T)      // just a pointer
    {
        digestOfPointer(digest, value);
    }
    else static if (!hasIndirections!T) // no pointers left in `T`
    {
        digestRaw(digest, value); // hash everything in one call for better speed
    }
    else static if (isArray!T) // including strings, wstring, dstring
    {
        digestOfArray(digest, value);
    }
    else static if (is(T == struct))
    {
        digestOfStruct(digest, value);
    }
    else
    {
        static assert(0, "handle type " ~ T.stringof);
    }
}

/** Digest raw bytes of `values`. */
void digestRaw(Digest, T)(scope ref Digest digest,
                          in T value)
{
    digest.put((cast(ubyte*)&value)[0 .. value.sizeof]);
}

/** Digest the class `value`. */
void digestOfPointer(Digest, T)(scope ref Digest digest,
                                in T value)
    if (is(T == class))
{
    digestRaw(digest, value);
}

/** Digest the struct `value`. */
void digestOfStruct(Digest, T)(scope ref Digest digest,
                               in T value)
    if (is(T == struct))
{
    foreach (ref subValue; value.tupleof)
    {
        digestAny(digest, subValue);
    }
}

/** Digest the array `value`. */
void digestOfArray(Digest, T)(scope ref Digest digest,
                              in T value)
    if (isArray!T)
{
    alias E = typeof(T.init[0]);
    static if (!hasIndirections!E)
    {
        digest.put((cast(ubyte*)value.ptr)[0 .. value.length * value[0].sizeof]);
    }
    else
    {
        static assert(0, "handle array with element type " ~ T.stringof);
    }
}

/** Digest the string `value`. */
void digestOfSomeString(Digest, T)(scope ref Digest digest,
                                   in T value)
    if (isSomeString!T)
{
}

/** Get hash of `value`.
 */
hash_t HashOf(alias hasher, T)(in auto ref T value)
{
    import std.digest.digest : isDigest;
    import std.traits : hasMember;

    static if (__traits(compiles, { hash_t _ = hasher(value); }))
    {
        return hasher(value);   // for instance `hashOf`
    }
    else static if (__traits(compiles, { enum _ = isDigest!hasher; }) &&
                    isDigest!hasher &&
                    hasMember!(hasher, "get"))
    {
        import std.digest.digest : makeDigest;

        auto dig = makeDigest!(hasher);

        digestAny(dig, value);

        dig.finish();

        auto result = dig.get();

        static if (is(typeof(result) == typeof(return)))
        {
            return result;
        }
        else static if (is(typeof(result) == typeof(return)[2]))
        {
            return (result[0] ^
                    result[1]);
        }
        else
        {
            static assert(0, "Handle get() with return type " ~ typeof(result).stringof ~
                          " on " ~ size_t.sizeof.stringof ~ "-bit platform");
        }
    }
    else static if (__traits(compiles, { auto _ = hasher((ubyte[]).init); }))
    {
        // cast input `value` to `ubyte[]` and use std.digest API
        immutable digest = hasher((cast(ubyte*)&value)[0 .. value.sizeof]); // TODO ask forums when this isn't correct

        static assert(digest.sizeof >=
                      typeof(return).sizeof,
                      `Size of digest is ` ~ digest.sizeof
                      ~ ` but needs to be at least ` ~ typeof(return).sizeof.stringof);

        import std.traits : isUnsigned, isStaticArray;

        static if (isUnsigned!(typeof(digest)))
        {
            return cast(typeof(return))digest; // fast modulo calculation
        }
        else static if (isStaticArray!(typeof(digest)))
        {
            typeof(return) hashIndex;

            static if (2*hash_t.sizeof == digest.sizeof)
            {
                // for instance, use all 128-bits when hash_t is 64-bit
                (cast(ubyte*)&hashIndex)[0 .. hashIndex.sizeof] = (digest[0 .. hashIndex.sizeof] ^
                                                                   digest[hashIndex.sizeof .. 2*hashIndex.sizeof]);
            }
            else
            {
                (cast(ubyte*)&hashIndex)[0 .. hashIndex.sizeof] = digest[0 .. hashIndex.sizeof];
            }

            return hashIndex;
        }
        else
        {
            static assert(0, "Unsupported digest type " ~ typeof(digest).stringof);
        }
    }
    else
    {
        static assert(0, "Cannot combine hasher " ~ hasher.stringof ~
                      " with element type " ~ T.stringof);
    }
}

hash_t hashOf2(alias hasher, T)(in auto ref T value)
{
    return HashOf!(hasher)(value);
}

@trusted pure unittest
{
    import digestx.fnv : FNV;

    alias FNV64 = FNV!(64, true);

    const ubyte[8] bytes8 = [1, 2, 3, 4, 5, 6, 7, 8];
    assert(hashOf2!(FNV64)(bytes8) == 9130222009665091821UL);

    struct V
    {
        float f = 3;
        double d = 5;
        real r = 7;
    }

    struct S
    {
        string str = `abc`;
        wstring wstr = `XYZ`;
        dstring dstr = `123`;
        size_t sz = 17;
        ushort us = 18;
        ubyte ub = 255;
        V v;
    }

    assert(hashOf2!(FNV64)("alpha") ==
           hashOf2!(FNV64)("alpha".dup));

    assert(hashOf2!(FNV64)(S()) ==
           hashOf2!(FNV64)(S()));
}

version(unittest)
{
    import dbgio;
}
