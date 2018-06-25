module enum_ex;

/** relation Direction.
 */
@safe:

/** Enumeration wrapper that uses optimized conversion to string (via `toString`
 * member).
 *
 * See_Also: https://forum.dlang.org/thread/ppndhxvzayedgpbjculm@forum.dlang.org?page=1
 */
struct Enum(E)
if (is(E == enum))
{
    @property string toString() @safe pure nothrow @nogc
    {
        import std.meta : AliasSeq;
        alias members = AliasSeq!(__traits(allMembers, E));
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
