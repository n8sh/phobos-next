module array_help;

/** Returns: statically (stack) allocated array with elements of type `T` of
    length `n`.

    For more convenient usage alias it as `s' together with UFCS for the
    following convenient notation:

    const x = [1, 2, 3].s;

    TODO Add to Phobos `std.array`.

    TODO Fix problems discussed here: http://forum.dlang.org/post/otrsanpgmokzpzqmfyvx@forum.dlang.org
    TODO File a bug report: http://forum.dlang.org/post/otrsanpgmokzpzqmfyvx@forum.dlang.org
*/
T[n] asStatic(T, size_t n)(T[n] arr)
{
    import std.traits : isCopyable;
    static if (isCopyable!T)
    {
        return arr;
    }
    else
    {
        static assert(false, "Support forwarding of uncopyable elements");
    }
}
alias s = asStatic;

version(unittest)
{
    /// non-copyable element type
    private static struct S
    {
        @disable this(this);
        int x;
    }
}

@safe pure nothrow @nogc:

/// non-copyable element type in static array
unittest
{
    // TODO this should compile: auto b = [S(42)].s;
}

///
unittest
{
    auto x = [1, 2, 3].asStatic;

    static assert(is(typeof(x) == int[x.length]));
    static assert(is(typeof([1, 2, 3].asStatic) == int[x.length]));

    static assert(!__traits(compiles,
                            {
                                static int[] doNotDoThat() { return [1, 2, 3].s; }
                            }
                      ));
}
