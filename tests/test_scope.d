@safe:

/** See also: http://forum.dlang.org/post/hwfpmabyunqhlkaqogdt@forum.dlang.org
    See also: https://issues.dlang.org/show_bug.cgi?id=17388
 */
struct S(T)
{
    scope inout(T)[] opSlice() inout return
    {
        return x[];
    }

    scope inout(Range) range() inout return
    {
        return typeof(return)(&this);
    }

    scope ref inout(T) first() inout return
    {
        return x[0];
    }

    scope inout(T)* pointer() inout return
    {
        return &x[0];
    }

    static private struct Range
    {
        S!T* _parent;
    }

    T[128] x;
}

@safe pure nothrow @nogc:

/// this correctly fails
int[] testSlice()
{
    S!int s;
    // TODO static assert(__traits(compiles, { return s[]; }));
    return s[];                 // should error with -dip1000
}

/// this should fail
auto testRange()
{
    S!int s;
    return s.range;
}

/// this should fail
ref int testFirst()
{
    S!int s;
    return s.first;             // should error with -dip1000
}

/// this should fail
int* testPointer()
{
    S!int s;
    return s.pointer;           // should error with -dip1000
}
