module emplace_all;

/** Version of `std.algorithm.mutation.moveEmplaceAll` that works for uncopyable
 * element type `T`.
 */
void moveEmplaceAllGeneric(T)(T[] src,
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
        }
    }
}
