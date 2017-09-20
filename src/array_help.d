module array_help;

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
*/
T[n] asStaticArray(T, size_t n)(T[n] x)
{
    import std.traits : isCopyable;
    static if (isCopyable!T)    // TODO remove when compiler moves this
    {
        return x;
    }
    else                        // TODO remove when compiler moves this
    {
        T[n] y = void;        // initialized below
        // TODO why doesn't this work here?
        // import std.algorithm.mutation : moveEmplaceAll;
        // moveEmplaceAll(x[], y[]);
        foreach (const ix, ref value; x)
        {
            import std.algorithm.mutation : move;
            move(value, y[ix]);
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

unittest
{
    auto x = [1, 2, 3].asStaticArray;
    static assert(is(typeof(x) == int[x.length]));
    static assert(is(typeof([1, 2, 3].asStaticArray) == int[x.length]));
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
