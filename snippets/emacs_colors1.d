import std.algorithm;

@safe pure:

alias X = int;

ref const(X) identity(ref return X x)
{
    return x;
}

@safe pure unittest
{
    X x = 42;
    assert(x == identity(x));
}
