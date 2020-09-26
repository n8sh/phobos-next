@safe pure unittest
{
    alias A = int[int];
    A x;
    x[0] = 0;
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", A.sizeof);
    assert(x is null);
    assert(!x);
    assert(1 !in x);
    x[31] = 13;
    assert(x !is null);
    assert(x);
}
