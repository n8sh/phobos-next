module math_ex;

import std.traits : isIntegral, isNumeric;

/** Check if `x` is an exact (binary) power of 2.
    See_Also: http://forum.dlang.org/thread/zumhmosfkvwjymjhmtlt@forum.dlang.org#post-fvnmurrctavpfkunssdf:40forum.dlang.org
    See_Also: http://forum.dlang.org/post/hloonbgclzloqemycnth@forum.dlang.org
*/
bool isPow2(T)(T x)
    if (isNumeric!T)
{
    import std.math : isPowerOf2; // https://github.com/dlang/phobos/pull/4327/files
    return isPowerOf2(x);
}
alias isPowerOf2 = isPow2;

/// ditto
bool isPow2A(T)(T x) if (isIntegral!T)
{
    return x && !(x & (x - 1));
}

/// ditto
bool isPow2B(T)(T x) if (isIntegral!T)
{
    return (x & -x) > (x - 1);
}

bool isPow2D(T)(T x) if (isIntegral!T)
{
    return (x > 0) && !(x & (x - 1));
}

/// ditto, avoids a jump instruction.
bool isPow2E(T)(T x) if (isIntegral!T)
{
    return (x > 0) & !(x & (x - 1));
}

/// ditto
bool isPow2F(T)(T x) if (isIntegral!T)
{
    return (x & -x) > (x >>> 1);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    foreach (f; AliasSeq!(isPow2, isPow2A, isPow2D, isPow2E, isPow2F))
    {
        // run-time
        assert(!f(7));
        assert(f(8));
        assert(!f(9));

        // compile-time
        static assert(!f(7));
        static assert(f(8));
        static assert(!f(9));

        assert(!f(0));
        assert(f(1));
        assert(f(2));
        assert(!f(3));
        assert(f(4));
        assert(!f(5));
        assert(!f(6));
        assert(!f(7));
        assert(f(8));
    }
}
