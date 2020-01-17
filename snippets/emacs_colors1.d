import std.algorithm;

@safe pure:

alias X = int;

ref const(X) identity(ref return const(X) x)
{
    return x;
}

@safe pure unittest
{
    const X x = 42;
    assert(x == identity(x));
}
