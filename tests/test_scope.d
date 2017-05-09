@safe pure nothrow @nogc:

struct S
{
    @safe pure nothrow @nogc

    auto opSlice() return scope
    {
        return x[];
    }

    int[4] x;
}

int[] f()
{
    S s;
    return s[];                 // should error with -dip1000
}

int[] g()()
{
    S s;
    return s.x[];               // errors by default
}

unittest
{
    f();
    static assert(!__traits(compiles, { g(); }));
}
