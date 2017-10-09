/** See also: http://forum.dlang.org/post/hwfpmabyunqhlkaqogdt@forum.dlang.org
    See also: https://issues.dlang.org/show_bug.cgi?id=17388
 */
struct S(T)
{
    scope inout(T)[] opSlice() inout return
    {
        return x[];
    }

    scope ref inout(T) first() inout return
    {
        return x[0];
    }

    T[4] x;
}

int[] testSlice() @safe pure nothrow @nogc
{
    S!int s;
    // TODO static assert(__traits(compiles, { return s[]; }));
    return s[];                 // should error with -dip1000
}

ref int testFirst() @safe pure nothrow @nogc
{
    S!int s;
    return s.first;             // should errir with -dip1000
}
