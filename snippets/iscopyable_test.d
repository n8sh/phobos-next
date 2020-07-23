import std.traits : isCopyable;

inout(char)[] f(inout(char)[] x)
{
    auto y = x;
    static assert(__traits(isCopyable, typeof(x))); // passes
    static assert(isCopyable!(typeof(x))); // used to fail
    return x;
}
