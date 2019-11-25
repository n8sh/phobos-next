/**
 * See_Also: https://forum.dlang.org/post/itlbdbdnybtyheyxdunq@forum.dlang.org
 */

@safe:

class C
{
@safe pure:
    this(int x)
    {
        this.x = x;
    }
    int x;
}

@trusted unittest
{
    C f()
    {
        import std.typecons : scoped;
        auto x = scoped!C(42);
        return x;
    }
    auto c = f();
    c.x = 42;                   // invalid memory access
}

C leakClass() @safe pure
{
    scope x = new C(42);
    return x;
}

@safe pure unittest
{
    auto x = leakClass();
}
