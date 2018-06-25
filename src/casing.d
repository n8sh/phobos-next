module casing;

import std.traits : isSomeString;

version(unittest)
{
    import std.algorithm : equal;
}

/** Convert string $(S s) to lower-case.
 *
 * String must contain ASCII characters only.
 */
auto toLowerASCII(S)(S s)
if (isSomeString!S)
{
    import std.algorithm.iteration : map;
    import std.ascii : toLower;
    import std.traits : isNarrowString;
    import std.utf : byUTF;

    // TODO functionize
    static if (isNarrowString!S) return s.byUTF!dchar.map!(ch => ch.toLower);
    else                         return t.map!(ch => ch.toLower);
}

///
@safe pure /*TODO nothrow @nogc*/ unittest
{
    assert("Lasse".toLowerASCII.equal("lasse"));
    assert("Åberg".toLowerASCII.equal("Åberg")); // ignores unicode letters
}

/** Convert string $(S s) to lower-case.
 *
 * String may contain Unicode characters.
 */
auto toLowerUnicode(S)(S s)
if (isSomeString!S)
{
    import std.algorithm.iteration : map;
    import std.uni : toLower;
    import std.traits : isNarrowString;
    import std.utf : byUTF;

    // TODO functionize
    static if (isNarrowString!S) return s.byUTF!dchar.map!(ch => ch.toLower);
    else                         return t.map!(ch => ch.toLower);
}

///
@safe pure /*TODO nothrow @nogc*/ unittest
{
    assert("Lasse".toLowerUnicode.equal("lasse"));
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

/** Convert enumeration value (enumerator) $(D t) to a range chars.
 */
auto toLowerSpacedChars(T, Separator)(const T t,
                                      const Separator separator = " ")
if (is(T == enum))
{
    import enum_ex : toStringFaster;
    return t.toStringFaster
            .camelCasedToLowerSpaced(separator);
}

///
@safe pure unittest
{
    enum Things { isUri, isLink }
    assert(Things.isUri.toLowerSpacedChars.equal("is uri"));
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
