module hacks;

import std.traits : isFunctionPointer, isDelegate, functionAttributes, FunctionAttribute, SetFunctionAttributes, functionLinkage;

/** Return `T` assumed to be `pure`.
 *
 * Copied from: https://dlang.org/phobos/std_traits.html#SetFunctionAttributes.
 * See also: https://forum.dlang.org/post/hmucolyghbomttqpsili@forum.dlang.org
 */
auto assumePure(T)(T t)
    if (isFunctionPointer!T || isDelegate!T)
{
    enum attrs = functionAttributes!T | FunctionAttribute.pure_;
    return cast(SetFunctionAttributes!(T, functionLinkage!T, attrs)) t;
}

int f(int x)
{
    return x + 1;
}

void g() pure
{
    static assert(!__traits(compiles, { auto x = f(42); }));
    auto pureF = assumePure(&f);
    assert(pureF(42) == 43);
}
