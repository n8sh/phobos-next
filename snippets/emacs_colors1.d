import std.algorithm;

@safe pure:

alias X = int;

ref const(X) identity(ref return X x)
{
    return x;
}
