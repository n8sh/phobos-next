@safe:

/** See also: http://forum.dlang.org/post/hwfpmabyunqhlkaqogdt@forum.dlang.org
    See also: https://issues.dlang.org/show_bug.cgi?id=17388
 */
struct S(T)
{
    static private struct Range
    {
        S!T* _parent;
    }

    scope inout(Range) range() inout return
    {
        return typeof(return)(&this);
    }

    scope inout(T)[] opSlice() inout return
    {
        return x[];
    }

    scope inout(T)[] slice() inout return
    {
        return x[];
    }

    scope ref inout(T) front() inout return
    {
        return x[0];
    }

    scope inout(T)* pointer() inout return // TODO should this be marked @system?
    {
        return &x[0];
    }

    T[128] x;
}

@safe pure nothrow @nogc:

/// this correctly fails
int[] testOpSlice()
{
    S!int s;
    return s[];                 // errors with -dip1000
}

/// this correctly fails
int[] testSlice()
{
    S!int s;
    return s.slice;             // errors with -dip1000
}

/// this correctly fails
auto testRange()
{
    S!int s;
    return s.range;             // errors with -dip1000
}

/// TODO this should fail
ref int testFront()
{
    S!int s;
    return s.front;             // should error with -dip1000
}

/// TODO this should fail
int* testPointer()
{
    S!int s;
    return s.pointer;           // should error with -dip1000
}
