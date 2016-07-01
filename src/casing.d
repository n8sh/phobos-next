module casing;

import std.traits : isSomeString;

/** Convert D-style camel-cased string $(S s) to lower-cased words.
 */
auto camelCasedToLower(S)(const S s)
    if (isSomeString!S)
{
    import std.algorithm.iteration : map;
    import std.ascii : isUpper; // D symbol names can only be in ASCII
    // TODO Instead of this add std.ascii.as[Lower|Upper]Case and import std.ascii.asLowerCase
    import std.uni : asLowerCase;
    import slicing : preSlicer;
    return s.preSlicer!isUpper.map!asLowerCase;
}

version(unittest)
{
    import std.algorithm : equal;
    import std.conv : to;
}

///
@safe pure unittest
{
    auto x = "doThis".camelCasedToLower;
    assert(equal(x.front, "do"));
    x.popFront;
    assert(equal(x.front, "this"));
}

/** Convert D-Style camel-cased string $(S s) to space-separated lower-cased words.
 */
auto camelCasedToLowerSpaced(S, Separator)(const S s, const Separator separator = " ")
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
    assert(equal(Things.isURI.toLowerSpacedChars, "is uri"));
    assert(equal(Things.isLink.toLowerSpacedChars, "is link"));
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
