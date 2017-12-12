module assuming;

/**
   * See_Also: http://forum.dlang.org/post/nq4eol$2h34$1@digitalmars.com
   * See also: https://dpaste.dzfl.pl/8c5ec90c5b39
   */
void assumeNogc(alias Func, T...)(T xs)
    @nogc
{
    import std.traits : isFunctionPointer, isDelegate, functionAttributes, FunctionAttribute, SetFunctionAttributes, functionLinkage;
    static auto assumeNogcPtr(T)(T f)
        if (isFunctionPointer!T ||
            isDelegate!T)
    {
        enum attrs = functionAttributes!T | FunctionAttribute.nogc;
        return cast(SetFunctionAttributes!(T, functionLinkage!T, attrs)) f;
    } {}
    assumeNogcPtr(&Func!T)(xs);
}
