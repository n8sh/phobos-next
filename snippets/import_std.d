import std;

auto f(T)(T x) pure
{
    return x;
}

@safe pure unittest
{
    const _ = f(42);
}
