module emplace_all;

/** Version of `std.algorithm.mutation.moveEmplaceAll` that works for uncopyable
 * element type `T`.
 */
void moveEmplaceAllGeneric(T)(T[] src,
                              T[] tgt)
{
    const n = src.length;
    assert(n == tgt.length);
    foreach (i; 0 .. n)
    {
        moveEmplace(src[i], tgt[i]);
    }
}
