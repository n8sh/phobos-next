module inplace_substitution;

import std.ascii : isASCII;

/** Substitute `from` by `to` in-place in `source`. */
void substituteInPlaceASCI(dchar from, dchar to)(scope char[] source)
    @safe pure nothrow @nogc
if (isASCII(from) &&
    isASCII(to))
{
    foreach (ref char ch; source)
    {
        if (ch == from)
        {
            ch = to;
        }
    }
}

@safe pure unittest
{
    auto x = "_a_b_c_";
    char[7] y = x;
    y.substituteInPlaceASCI!('_', ' ');
    assert(y[] == " a b c ");
}
