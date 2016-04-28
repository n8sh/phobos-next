module math_ex;

import std.traits : isIntegral;

/** Check if `x` is an exact (binary) power of 2.
    See also: http://forum.dlang.org/thread/zumhmosfkvwjymjhmtlt@forum.dlang.org#post-fvnmurrctavpfkunssdf:40forum.dlang.org
    See also: http://forum.dlang.org/post/hloonbgclzloqemycnth@forum.dlang.org
    TODO Move to Phobos std.math.
*/
bool isPow2(T)(T x)
    if (isIntegral!T)
{
    import core.bitop : popcnt;
    // TODO Use popcnt if available otherwise (x & -x) > (x - 1);
    // return (x & -x) > (x - 1);
    return popcnt(x) == 1;
}
alias isPowerOf2 = isPow2;

@safe pure nothrow @nogc unittest
{
    // run-time
    assert(!7.isPow2);
    assert(8.isPow2);
    assert(!9.isPow2);

    // compile-time
    static assert(!7.isPow2);
    static assert(8.isPow2);
    static assert(!9.isPow2);

    assert(!0.isPow2);
    assert(1.isPow2);
    assert(2.isPow2);
    assert(!3.isPow2);
    assert(4.isPow2);
    assert(!5.isPow2);
    assert(!6.isPow2);
    assert(!7.isPow2);
    assert(8.isPow2);
}
