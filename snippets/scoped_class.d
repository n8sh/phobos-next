@safe:

class C
{
@safe pure:
    this()
    {
    }
}

@trusted unittest
{
    import std.typecons : scoped;
    auto x = scoped!C();
}
