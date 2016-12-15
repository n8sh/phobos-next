module casing;

import std.traits : isSomeString;

version(unittest)
{
    import std.algorithm : equal;
    import std.conv : to;
}

/** Convert string $(S s) to lower-case.
    String may contain only ASCII letters.
 */
auto toLowerASCII(S)(S s)
    if (isSomeString!S)
{
    import std.algorithm.iteration : map;
    import std.ascii : toLower;
    return s.map!(ch => ch.toLower);
}

/** Convert string $(S s) to lower-case.
    String may contain unicode letters.
 */
auto toLowerUnicode(S)(S s)
    if (isSomeString!S)
{
    import std.algorithm.iteration : map;
    import std.uni : toLower;
    return s.map!(ch => ch.toLower);
}

///
@safe pure /*TODO nothrow @nogc*/ unittest
{
    assert("Lasse".toLowerASCII.equal("lasse"));
    assert("Åberg".toLowerUnicode.equal("åberg"));
}

/** Convert D-style camel-cased string $(S s) to lower-cased words.
 */
auto camelCasedToLower(S)(S s)
    if (isSomeString!S)
{
    import std.algorithm.iteration : map;
    import std.ascii : isUpper; // D symbol names can only be in ASCII
    // TODO Instead of this add std.ascii.as[Lower|Upper]Case and import std.ascii.asLowerCase
    import std.uni : asLowerCase;
    import slicing : preSlicer;
    return s.preSlicer!isUpper.map!asLowerCase;
}

///
@safe pure unittest
{
    auto x = "doThis".camelCasedToLower;
    assert(x.front.equal("do"));
    x.popFront();
    assert(x.front.equal("this"));
}

/** Convert D-Style camel-cased string $(S s) to space-separated lower-cased words.
 */
auto camelCasedToLowerSpaced(S, Separator)(S s, const Separator separator = " ")
    if (isSomeString!S)
{
    import std.algorithm.iteration : joiner;
    return camelCasedToLower(s).joiner(separator);
}

///
@safe pure unittest
{
    assert(equal("doThis".camelCasedToLowerSpaced,
                 "do this"));
}

/** Convert Enumeration Value (Enumerator) $(D t) to a range chars.
 */
auto toLowerSpacedChars(T, Separator)(const T t, const Separator separator = " ")
    if (is(T == enum))
{
    import std.conv : to;
    return t.to!(const(char)[]) // don't need immutable here
            .camelCasedToLowerSpaced(separator);
}

///
@safe pure unittest
{
    enum Things { isURI, isLink }
    assert(Things.isURI.toLowerSpacedChars.equal("is uri"));
    assert(Things.isLink.toLowerSpacedChars.equal("is link"));
}

///
@safe pure unittest
{
    enum Things { isURI, isLink }
    auto r = Things.isURI.toLowerSpacedChars;
    alias R = typeof(r);
    import std.range : ElementType;
    alias E = ElementType!R;
    static assert(is(E == dchar));
}
