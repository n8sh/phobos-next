import std.variant : Algebraic;

///
@safe pure unittest
{
    alias A = Algebraic!(int,
                         const(long),
                         immutable(float));
    A x;
}

///
@safe pure unittest
{
    alias A = Algebraic!(int, long);
    static immutable A x;
}
