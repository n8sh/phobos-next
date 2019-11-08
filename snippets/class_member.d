struct S
{
    int x;
    string y;
}

auto field(string fieldName, T)(T x)
if (is(T == struct))
{
    mixin("return x." ~ fieldName ~ ";");
}

@safe pure nothrow @nogc unittest
{
    auto s = S(42, "abc");
    assert(s.field!"x" == 42);
    assert(s.field!"y" == "abc");
}
