@safe pure nothrow @nogc:

/** See also: http://forum.dlang.org/post/hwfpmabyunqhlkaqogdt@forum.dlang.org
    See also: https://issues.dlang.org/show_bug.cgi?id=17388
 */
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

// void g()
// {
//     int[] r;
//     S s;
//     static assert(!__traits(compiles, { r = s[]; }));
// }
