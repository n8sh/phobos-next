import std.variant : Algebraic;

///
@safe pure unittest
{
    alias A = Algebraic!(int,
                         const(long),
                         immutable(int*));
    A x;
}

/// assignment is unsafe and unpure
unittest
{
    alias A = Algebraic!(int,
                         const(long),
                         immutable(int*));
    A x;
    x = x;
    x = 42;
    assert(x == 42);
}

///
@safe pure unittest
{
    alias A = Algebraic!(int, string);
    static immutable A x;
    A y;
}
