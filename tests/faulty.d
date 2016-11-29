module faulty;

auto square(T)(T x)
{
    return x*x;
}

auto f() {}
auto g() {}
auto h() {}
auto i() {}
auto j() {}
auto k() {}
auto l() {}
auto m() {}

@safe pure nothrow @nogc unittest
{
    assert(square(2) == 4);
    assert(square(3) == 9);
    assert(square(4) == 15, "Some failure");
}
