module array_help;

import std.traits : Unqual;

/** Returns: statically (stack) allocated array with elements of type `T` of
    length `n`.

    For more convenient usage alias it as `s' together with UFCS for the
    following convenient notation:

    const x = [1, 2, 3].s;

    TODO Add to Phobos `std.array`.

    TODO Fix problems discussed here: http://forum.dlang.org/post/otrsanpgmokzpzqmfyvx@forum.dlang.org
    TODO File a bug report: http://forum.dlang.org/post/otrsanpgmokzpzqmfyvx@forum.dlang.org

    TODO fix compiler so that move kicks in here automatically and remove
    special case on `isCopyable`

    See also: http://dpaste.dzfl.pl/d0059e6e6c09
    See also: http://forum.dlang.org/post/oq0cd1$2ji3$1@digitalmars.com
*/
Unqual!T[n] asStaticArray(T, size_t n)(T[n] x...) @trusted
{
    import std.traits : isCopyable, hasElaborateDestructor; // TODO remove `move` when compiler does it for us
    static if (isCopyable!T)  // TODO remove `move` when compiler does it for us
    {
        return x[];
    }
    else                      // TODO remove `move` when compiler does it for us
    {
        // TODO remove `move` when compiler does it for us:
        T[n] y = void;        // initialized below
        static if (hasElaborateDestructor!T)
        {
            /* NOTE: moveEmplaceAll doesn't support uncopyable elements
             * import std.algorithm.mutation : moveEmplaceAll;
             * moveEmplaceAll(x[], y[]);
             */
            foreach (const ix, ref value; x)
            {
                import std.algorithm.mutation : move;
                move(value, y[ix]);
            }
        }
        else
        {
            import core.stdc.string : memcpy;
            memcpy(y.ptr, x.ptr, n*T.sizeof); // fast
        }
        return y;
    }
}
alias s = asStaticArray;

version(unittest)
{
    private static struct US
    {
        @disable this(this);
        int x;
    }
}

///
@safe pure nothrow @nogc unittest
{
    auto a = [1, 2, 3].asStaticArray;
    static assert(is(typeof(a) == int[a.length]));
    static assert(is(typeof([1, 2, 3].asStaticArray) == int[a.length]));

    auto b = "hello".s;
    static assert(is(typeof(b) == char[5]));

    auto x = s!ubyte(1, 2, 3);
    static assert(is(typeof(x) == ubyte[3]));
}

/// non-copyable element type in static array
@safe pure nothrow @nogc unittest
{
    auto b = [US(42)].s;
}

///
@safe pure nothrow @nogc unittest
{
    auto x = [1, 2, 3].asStaticArray;

    static assert(is(typeof(x) == int[x.length]));
    static assert(is(typeof([1, 2, 3].asStaticArray) == int[x.length]));

    static assert(!__traits(compiles,
                            {
                                static int[] doNotDoThat() { return [1, 2, 3].s; }
                            }
                      ));
}

/** Returns: `x` as a static array of unsigned bytes. */
pragma(inline, true)
@property ubyte[T.sizeof] toUbytes(T)(in T x)
    @trusted pure nothrow @nogc // TODO endian-dependent
{
    return (cast(ubyte*)(&x))[0 .. x.sizeof];
}

/** Returns: `x` as a static array with elements of type `E`. */
pragma(inline, true)
@property ref E[T.sizeof] asN(E, T)(in ref T x)
    @trusted pure nothrow @nogc // TODO endian-dependent
    if (T.sizeof % E.sizeof == 0)
{
    return (cast(E*)(&x))[0 .. x.sizeof];
}

@safe pure nothrow @nogc unittest
{
    immutable ushort x = 17;
    auto y = x.asN!ubyte;
    version(LittleEndian)
    {
        assert(y == [17, 0].s);
    }
}

private enum wordBytes = size_t.sizeof;
private enum wordBits = 8*wordBytes;

/** Returns: number of words (`size_t`) needed to represent
 * `_bins.length` holes.
 */
private static size_t holesWordCount(size_t bitCount)
    @safe pure nothrow @nogc
{
    return (bitCount / wordBits +
            (bitCount % wordBits ? 1 : 0));
}

private static size_t binBlockBytes(size_t bitCount)
    @safe pure nothrow @nogc
{
    return wordBytes*holesWordCount(bitCount);
}

size_t* makeUninitializedBitArray(alias Allocator)(size_t bitCount)
    @trusted pure nothrow @nogc
{
    return cast(typeof(return))Allocator.instance.allocate(binBlockBytes(bitCount));
}

size_t* makeZeroBitArray(alias Allocator)(size_t bitCount)
    @trusted pure nothrow @nogc
    if (__traits(hasMember, Allocator, "zeroallocate"))
{
    return cast(typeof(return))Allocator.instance.zeroallocate(binBlockBytes(bitCount));
}

@trusted pure unittest
{
    import pure_mallocator : PureMallocator;

    const bitCount = 8*size_t.sizeof + 1;
    const wordCount = 2;

    size_t* x = makeUninitializedBitArray!(PureMallocator)(bitCount);
    PureMallocator.instance.deallocate(cast(void[])(x[0 .. wordCount]));

    size_t* y = makeZeroBitArray!(PureMallocator)(bitCount);
    PureMallocator.instance.deallocate(cast(void[])(y[0 .. wordCount]));
}
