module emplace_all;

/** Version of `std.algorithm.mutation.moveEmplaceAll` that works for uncopyable
 * element type `T`.
 */
void moveEmplaceAllNoReset(T)(T[] src,
                              T[] tgt)
{
    import container_traits : needsMove;
    const n = src.length;
    assert(n == tgt.length);
    foreach (i; 0 .. n)
    {
        static if (needsMove!T)
        {
            import std.algorithm.mutation : moveEmplace;
            moveEmplace(src[i], tgt[i]);
        }
        else
        {
            tgt[i] = src[i];
            src[i] = T.init;    // TODO remove later on
        }
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
