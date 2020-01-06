import std.variant : Algebraic;

///
@safe pure unittest
{
    alias Ail = Algebraic!(int,
                           const(long),
                           immutable(float));
    immutable Ail _;
}

///
@safe pure unittest
{
    alias Ail = Algebraic!(int, long);
    static immutable Ail _;
}
