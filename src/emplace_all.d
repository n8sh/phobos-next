module emplace_all;

/** Version of `std.algorithm.mutation.moveEmplaceAll` that works for uncopyable
 * element type `T`.
 */
void moveEmplaceAllNoReset(T)(scope T[] src,
                              scope T[] tgt)
{
    assert(src.length == tgt.length);
    import container_traits : needsMove;
    static if (needsMove!T)
    {
        immutable n = src.length;
        // TODO benchmark with `memmove` and `memset` instead
        foreach (i; 0 .. n)
        {
            import std.algorithm.mutation : moveEmplace;
            moveEmplace(src[i], tgt[i]);
        }
    }
    else
    {
        tgt[] = src[];
        src[] = T.init;
    }
}

@trusted pure nothrow @nogc unittest
{
    import uncopyable_sample : SomeUncopyable;

    alias T = SomeUncopyable;
    enum n = 3;
    alias A = T[n];

    A x = [T(1), T(2), T(3)];

    A y = void;
    moveEmplaceAllNoReset(x[], y[]);

    foreach (i; 0 .. n)
    {
        assert(x[i] == T.init);
        assert(*y[i].valuePointer == i + 1);
    }
}
