module inplace_substitution;

import std.ascii : isASCII;

@safe pure nothrow @nogc:

/** Substitute `from` by `to` in-place in `source`. */
void substituteInPlaceASCII(dchar from, dchar to)(scope char[] source) 
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

/** Returns: in-place substitution `from` by `to` in `source`. */
char[] substitutedInPlaceASCII(dchar from, dchar to)(return scope char[] source) 
if (isASCII(from) &&
    isASCII(to))
{
    substituteInPlaceASCII!(from, to)(source);
    return source;
}

///
unittest
{
    const x = "_a_b_c_";

    char[7] y = x;
    y.substituteInPlaceASCII!('_', ' ');
    assert(y[] == " a b c ");
}
