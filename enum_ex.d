module enum_ex;

@safe:

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
    enum X { a,
             b,
             // _b = b             // enumerator alias
    }
    alias EnumX = Enum!X;
}
