module hash_ex;

/** Get hash of `value`.
 */
pragma(inline)              // LDC can inline, DMD cannot
size_t HashOf(alias hasher, T)(in T value)
{
    import std.digest.digest : isDigest;
    import std.traits : hasMember;

    static if (__traits(compiles, { size_t _ = hasher(value); }))
    {
        return hasher(value);   // for instance `hashOf`
    }
    else static if (__traits(compiles, { enum _ = isDigest!hasher; }) &&
                    isDigest!hasher &&
                    hasMember!(hasher, "get"))
    {
        import std.digest.digest : makeDigest;

        auto dig = makeDigest!(hasher);

        // TODO use:
        // static if (hasMember!(hasher, "putStaticArray"))
        // {
        //     dig.putStaticArray((cast(ubyte*)&value)[0 .. value.sizeof]);
        // }
        // else
        // {
        //     dig.put((cast(ubyte*)&value)[0 .. value.sizeof]);
        // }

        import std.traits : isScalarType, isAggregateType, hasIndirections, isSomeString, isArray;

        static if (isScalarType!T)
        {
            dig.put((cast(ubyte*)&value)[0 .. value.sizeof]);
        }
        else static if (isArray!T) // including strings, wstring, dstring
        {
            alias E = typeof(T.init[0]);
            static if (!hasIndirections!E)
            {
                dig.put((cast(ubyte*)value.ptr)[0 .. value.length * value[0].sizeof]);
            }
            else
            {
                static assert(0, "handle array when element type " ~ T.stringof);
            }
        }
        else static if (isAggregateType!T)
        {
            foreach (ref subValue; value.tupleof)
            {
                alias ST = typeof(subValue);
                static if (hasIndirections!ST)
                {
                    static if (isArray!ST)
                    {
                        alias STE = typeof(ST.init[0]);
                        static if (!hasIndirections!STE)
                        {
                            dig.put((cast(ubyte*)subValue.ptr)[0 .. subValue.length * subValue[0].sizeof]);
                        }
                        else
                        {
                            static assert(0, "handle array when element type " ~ T.stringof);
                        }
                    }
                    else
                    {
                        static assert(0, "handle type " ~ ST.stringof);
                    }
                }
                else
                {
                    dig.put((cast(ubyte*)&value)[0 .. value.sizeof]);
                }
            }
        }
        else
        {
            static assert(0, "handle type " ~ T.stringof);
        }

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

            static if (2*size_t.sizeof == digest.sizeof)
            {
                // for instance, use all 128-bits when size_t is 64-bit
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

size_t hashOf2(alias hasher, T)(in auto ref T value)
{
    import std.traits : hasIndirections;
    static if (hasIndirections!T)
    {
        import std.traits : FieldNameTuple;
        return HashOf!(hasher)(value);
    }
    else
    {
        return HashOf!(hasher)(value);
    }
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

    S s;

    // dln(hashOf2!(FNV64)(s));
    // dln(hashOf2!(FNV64)(s));
    // dln(hashOf2!(FNV64)(s));
    // dln(hashOf2!(FNV64)(s));
}

version(unittest)
{
    import dbgio;
}
