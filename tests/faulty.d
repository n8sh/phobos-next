module faulty;

auto square(T)(T x)
{
    if (x == 4)
    {
        return x*x - 1;         // bug
    }
    else
    {
        return x*x;
    }
}

auto f() {}
auto g() {}
auto h() {}
auto i() {}
auto j() {}
auto k() {}
auto l() {}
auto m() {}

@safe pure nothrow unittest
{
    assert(square(2) == 4);
    assert(square(3) == 9);
    assert(square(4) == 16, "Some specific failure");
}
