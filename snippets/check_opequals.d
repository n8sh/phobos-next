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
    @property bool opEquals(const scope typeof(this) rhs) const
    {
        return x == rhs.x;
    }
    int x;
}

int main(string[])
{
    static assert(__traits(hasMember, S, "opEquals"));
    static assert(__traits(hasMember, T, "opEquals"));
    return 0;
}
