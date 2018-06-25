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

///
@safe pure unittest
{
    enum X { a,
             b,
             _b = b             // enumerator alias
    }
    alias EX = Enum!X;
    assert(EX(X.a).toString == "a");
    assert(EX(X.b).toString == "b");
    assert(EX(X._b).toString == "b"); // alias encodes to original
}
