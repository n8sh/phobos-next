module enum_ex;

@safe:

struct Enum(E)
if (is(E == enum))
{
    @property string toString() @safe pure nothrow @nogc
    {
        // fast implementation
    }
    E _enum;
    alias _enum this;
}

@safe pure unittest
{
    enum X { a,
             b,
             _b = b             // enumerator alias
    }
}
