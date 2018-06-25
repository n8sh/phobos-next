module enum_ex;

@safe:

/** Enumeration wrapper that uses optimized conversion to string (via `toString`
 * member).
 */
struct Enum(E)
if (is(E == enum))
{
    @property string toString() @safe pure nothrow @nogc
    {
        enum members = [__traits(allMembers, E)];
        final switch (_enum)
        {
            static foreach (index, member; members)
            {
                static if (index == 0 ||
                           (__traits(getMember, E, members[index - 1]) !=
                            __traits(getMember, E, member)))
                {
                case __traits(getMember, E, member):
                    return member;
                }
            }
        }
    }
    E _enum;                    // the wrapped enum
    alias _enum this;
}

@safe pure unittest
{
    import std.conv : to;
    enum X { a,
             b,
             _b = b             // enumerator alias
    }
    alias EnumX = Enum!X;
    assert(EnumX(X.a).to!string == "a");
    assert(EnumX(X.b).to!string == "b");
    assert(EnumX(X._b).to!string == "b");
}
