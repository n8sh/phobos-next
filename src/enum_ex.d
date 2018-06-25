module enum_ex;

/** relation Direction.
 */
@safe:

/** Enumeration wrapper that uses optimized conversion to string (via `toString`
 * member).
 *
 * See_Also: https://forum.dlang.org/thread/ppndhxvzayedgpbjculm@forum.dlang.org?page=1
 *
 * TODO: Move logic to `std.conv.to`.
 */
struct Enum(E)
if (is(E == enum))
{
    @property string toString() @safe pure nothrow @nogc
    {
        import conv_ex : toStringFaster;
        return toStringFaster(_enum);
    }

    E theEnum() const @safe pure nothrow @nogc { return _enum; }

    E _enum;                    // the wrapped enum
    alias _enum this;
}

///
@safe pure unittest
{
    enum X { a,
             b,
             _b = b             // enumerator alias
    }
    alias EnumX = Enum!X;
    assert(EnumX(X.a).toString == "a");
    assert(EnumX(X.b).toString == "b");
    assert(EnumX(X._b).toString == "b"); // alias encodes to original
}
