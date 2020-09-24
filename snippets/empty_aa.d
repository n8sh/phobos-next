@safe pure unittest
{
    alias A = int[int];
    A x;
    assert(!x);
    assert(1 !in x);
    x[31] = 13;
    assert(x);
}
