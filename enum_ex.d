module enum_ex;

@safe:

/** Enumeration wrapper to uses optimized conversion to string (via `toString`
 * member).
 */
struct Enum(E)
if (is(E == enum))
{
    @property string toString() @safe pure nothrow @nogc
    {
        final switch (_enum)
        {
            static foreach (member; __traits(allMembers, E))
            {
            case __traits(getMember, E, member):
                return member;
            }
        }
    }
    E _enum;
    alias _enum this;
}

@safe pure unittest
{
    import std.conv : to;
    enum X { a,
             b,
             // _b = b             // enumerator alias
    }
    alias EnumX = Enum!X;
    assert(EnumX(X.a).to!string == "a");
    assert(EnumX(X.b).to!string == "b");
}
