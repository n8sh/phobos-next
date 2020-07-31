auto f(T)(T x) pure
{
    return x;
}

auto g(T)(T x) pure
{
    return f(x);
}

@safe pure unittest
{
    const _ = g(42);
}
