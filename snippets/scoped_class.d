/**
 * See_Also: https://forum.dlang.org/post/itlbdbdnybtyheyxdunq@forum.dlang.org
 */

class C
{
    @safe pure nothrow @nogc:
    this(int x)
    {
        this.x = x;
    }
    int x;
}

C leakClass() @safe pure nothrow
{
    scope x = new C(42);
    return x;
}

@safe pure nothrow unittest
{
    auto x = leakClass();
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
