/**
 * See_Also: https://forum.dlang.org/post/itlbdbdnybtyheyxdunq@forum.dlang.org
 */

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
