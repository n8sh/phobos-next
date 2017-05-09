@safe pure nothrow @nogc:

struct S
{
    @safe pure nothrow @nogc
    inout(int)[] opSlice() inout return scope
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
