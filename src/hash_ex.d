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
        return hasher(value);     // for instance `hashOf`
    }
    else static if (__traits(compiles, { enum _ = isDigest!hasher; }) &&
                    isDigest!hasher &&
                    hasMember!(hasher, "get"))
    {
        import std.digest.digest : makeDigest;

        auto dig = makeDigest!(hasher);
        dig.put((cast(ubyte*)&value)[0 .. value.sizeof]);
        dig.finish();

        static if (is(typeof(dig.get()) == typeof(return)))
        {
            return dig.get();
        }
        else static if (is(typeof(dig.get()) == typeof(return)[2]))
        {
            return (dig.get()[0] ^ dig.get()[1]);
        }
        else
        {
            static assert(0, "Handle get() with return type " ~ typeof(dig.get()).stringof ~
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
