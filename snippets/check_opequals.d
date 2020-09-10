struct S
{
    @property bool opEquals(const scope typeof(this) rhs) const
    {
        return x == rhs.x;
    }
    int x;
}

struct T
{
    int x;
}

@safe pure unittest
{
    static assert(__traits(hasMember, S, "opEquals"));
    static assert(!__traits(hasMember, T, "opEquals"));
}
