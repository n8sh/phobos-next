void test9994()
{
    static struct S
    {
        static int dtor;
        ~this() { ++dtor; }
    }

    S s;
    static assert( __traits(compiles, s.opAssign(s)));
    static assert(!__traits(compiles, s.__postblit()));

    assert(S.dtor == 0);
    s = s;
    assert(S.dtor == 1);
}
