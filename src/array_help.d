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
Unqual!T[n] asStaticArray(T, size_t n)(T[n] x) @trusted
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
            // TODO why doesn't this work here?
            // import std.algorithm.mutation : moveEmplaceAll;
            // moveEmplaceAll(x[], y[]);
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
    private static struct SomeUncopyableStruct
    {
        @disable this(this);
        int x;
    }
}

@safe pure nothrow @nogc:

///
unittest
{
    auto a = [1, 2, 3].asStaticArray;
    static assert(is(typeof(a) == int[a.length]));
    static assert(is(typeof([1, 2, 3].asStaticArray) == int[a.length]));
    auto b = "hello".s;
    static assert(is(typeof(b) == char[5]));
}

/// non-copyable element type in static array
unittest
{
    auto b = [SomeUncopyableStruct(42)].s;
}

///
unittest
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
