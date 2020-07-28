// import std;

auto f(T)(T x) pure
{
    return x;
}

auto g(T)(T x) pure
{
    return f(x);
}

alias X(T) = T;
alias Y = X!(int);

enum name(T) = T.stringof;

enum int_name = name!int;

@safe pure unittest
{
    const _ = g(42);
}
