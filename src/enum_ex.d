module enum_ex;

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
        return toStringFaster(_enum);
    }
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

/** Fast and more generic implementation of `std.conv.to` for enumerations.
 */
string toStringFaster(T)(T value) @safe pure nothrow @nogc
if (is(T == enum))
{
    import std.meta : AliasSeq;
    alias members = AliasSeq!(__traits(allMembers, T));
    final switch (value)
    {
        static foreach (index, member; members)
        {
            static if (index == 0 ||
                       (__traits(getMember, T, members[index - 1]) !=
                        __traits(getMember, T, member)))
            {
            case __traits(getMember, T, member):
                return member;
            }
        }
    }
}

///
@safe pure nothrow @nogc unittest
{
    enum E { unknown, x, y, z, }
    assert(E.x.toStringFaster == "x");
    assert(E.y.toStringFaster == "y");
    assert(E.z.toStringFaster == "z");
}
