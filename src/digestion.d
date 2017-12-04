/** Hash digestion of standard types.
 *
 * TODO use:
 *
 *  static if (hasMember!(hasher, "putStaticArray"))
 *  {
 *      digest.putStaticArray((cast(ubyte*)&value)[0 .. value.sizeof]);
 *  }
 *  else
 *  {
 *      digest.put((cast(ubyte*)&value)[0 .. value.sizeof]);
 *  }
 */
module digestion;

import std.traits : hasMember, isScalarType, hasIndirections, isArray, isPointer;
import std.digest.digest : isDigest;

pragma(inline, true):

/** Digest `value` into `digest`.
 */
void digestAny(Digest, T)(ref Digest digest,
                          in auto ref T value)
    if (isDigest!Digest)
{
    static if (hasMember!(T, "toDigest"))
    {
        value.toDigest(digest);
    }
    else static if (isScalarType!T)  // first because faster to evaluate than
                                // `!hasIndirections!T` below
    {
        digestRaw(digest, value);
    }
    else static if (is(T == class) && // a class is memory-wise
                    isPointer!T)      // just a pointer. consistent with opCmp
    {
        digestPointer(digest, value);
    }
    else static if (!hasIndirections!T) // no pointers left in `T`
    {
        digestRaw(digest, value); // hash everything in one call for better speed
    }
    else static if (isArray!T) // including strings, wstring, dstring
    {
        digestArray(digest, value);
    }
    else static if (is(T == struct))
    {
        import std.range : hasSlicing;
        static if (hasSlicing!T && isArray!(T.init[]))
        {
            // T is array container
            digestArray(digest, value[]);
        }
        else
        {
            digestStruct(digest, value);
        }
    }
    else
    {
        static assert(0, "handle type " ~ T.stringof);
    }
}

/** Digest the class `value`. */
private void digestPointer(Digest, T)(scope ref Digest digest,
                                      in T value) // pointer passed by value
    if (isDigest!Digest &&
        (is(T == class) ||
         isPointer!T))
{
    digestRaw(digest, value);
}

/** Digest the struct `value` by digesting each member sequentially. */
pragma(inline)                  // DMD cannot inline
private void digestStruct(Digest, T)(scope ref Digest digest,
                                     in auto ref T value)
    if (isDigest!Digest &&
        is(T == struct))
{
    static if (!hasIndirections!T)
    {
        digestRaw(digest, value); // hash everything in one call for better speed
    }
    else
    {
        foreach (const ref subValue; value.tupleof) // for each member
        {
            digestAny(digest, subValue);
        }
    }
}

/** Digest the array `value`. */
private void digestArray(Digest, T)(scope ref Digest digest,
                                    in auto ref T value) @trusted
    if (isDigest!Digest &&
        isArray!T)
{
    alias E = typeof(T.init[0]);
    static if (!hasIndirections!E)
    {
        // faster:
        digestRaw(digest, value.length);
        digest.put((cast(ubyte*)value.ptr)[0 .. value.length * value[0].sizeof]);
    }
    else
    {
        static assert(0, "handle array with element type " ~ T.stringof);
    }
}

/** Digest raw bytes of `values`. */
private void digestRaw(Digest, T)(scope ref Digest digest,
                                  in auto ref T value) @trusted
    if (isDigest!Digest)
{
    digest.put((cast(ubyte*)&value)[0 .. value.sizeof]);
}

/** Get hash of `value`.
 *
 * A faster alternative to `hashOf`.
 */
pragma(inline)                  // DMD cannot inline
hash_t hashOf2(alias hasher, T)(in auto ref T value)
{
    static if (__traits(compiles, { hash_t _ = hasher(value); }))
    {
        return hasher(value);   // for instance `hashOf`
    }
    else static if (__traits(compiles, { enum _ = isDigest!hasher; }) &&
                    isDigest!hasher &&
                    hasMember!(hasher, "get"))
    {
        import std.digest.digest : makeDigest;
        auto digest = makeDigest!(hasher);
        static if (hasMember!(T, "toDigest"))
        {
            value.toDigest(digest);
        }
        else
        {
            digestAny(digest, value);
        }
        digest.finish();
        auto result = digest.get();
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

@trusted pure unittest
{
    import digestx.fnv : FNV;

    alias FNV64 = FNV!(64, true);

    const ubyte[8] bytes8 = [1, 2, 3, 4, 5, 6, 7, 8];
    assert(hashOf2!(FNV64)(bytes8) == 9130222009665091821UL);

    enum E { first, second, third }

    assert(hashOf2!(FNV64)(E.first) ==
           hashOf2!(FNV64)(E.first));
    assert(hashOf2!(FNV64)(E.second) ==
           hashOf2!(FNV64)(E.second));
    assert(hashOf2!(FNV64)(E.third) ==
           hashOf2!(FNV64)(E.third));
    assert(hashOf2!(FNV64)(E.first) !=
           hashOf2!(FNV64)(E.second));
    assert(hashOf2!(FNV64)(E.second) !=
           hashOf2!(FNV64)(E.third));

    struct V
    {
        float f = 3;
        double d = 5;
        real r = 7;
        E e;
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

    // check determinism
    assert(hashOf2!(FNV64)("alpha") ==
           hashOf2!(FNV64)("alpha".dup));

    assert(hashOf2!(FNV64)(S()) ==
           hashOf2!(FNV64)(S()));

    struct HasDigest
    {
        E e;
        void toDigest(Digest)(scope ref Digest digest) const
            pure nothrow @nogc
            if (isDigest!Digest)
        {
            digestRaw(digest, e);
        }
    }

    assert(hashOf2!(FNV64)(HasDigest(E.first)) ==
           hashOf2!(FNV64)(HasDigest(E.first)));

    assert(hashOf2!(FNV64)(HasDigest(E.first)) !=
           hashOf2!(FNV64)(HasDigest(E.second)));
}

version(unittest)
{
    import dbgio;
}
