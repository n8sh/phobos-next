module emplace_all;

void moveEmplaceAllGeneric(T)(T[] src, T[] tgt)
{
    const n = src.length;
    assert(n == tgt.length);
    foreach (i; 0 .. n)
    {
        moveEmplace(src[i], tgt[i]);
    }
}
