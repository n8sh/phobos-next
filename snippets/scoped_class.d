@safe:

class C
{
@safe pure:
    this()
    {
    }
    int x;
}

@trusted unittest
{
    C f()
    {
        import std.typecons : scoped;
        auto x = scoped!C();
        return x;
    }
    auto c = f();
    c.x = 42;                   // invalid memory access
}
