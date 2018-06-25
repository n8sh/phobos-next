module conv_ex;

import std.traits: isSomeChar, isSomeString, CommonType;
import traits_ex : haveCommonType, isSourceOfSomeChar;
import std.range : isInputRange, ElementType;

/** Variant of std.conv.to with $(D defaultValue) making it $(D nothrow).
 *
 * See_Also: https://forum.dlang.org/post/bnbbheofzaxlabvnvrrc@forum.dlang.org
 * See_Also: http://forum.dlang.org/post/tsszfamjalzviqjhpdcr@forum.dlang.org
 * See_Also: https://forum.dlang.org/post/kdjbkqbnspzshdqtsntg@forum.dlang.org
 */
T toDefaulted(T, S, U)(scope const S value, /*lazy*/ U defaultValue)
if (!is(T == enum) &&
    is(typeof(() { T r = defaultValue; }))) // TODO use std.traits.isAssignable!(T, U) ?
{
    try
    {
        import std.conv : to;
        return value.to!T;
    }
    catch (Exception e) // assume `ConvException`. TODO can we capture `ConvException` instead make it inferred `nothrow`
    {
        return defaultValue;
    }
}

/// ditto
T toDefaulted(T)(scope const(char)[] value, T defaultValue) @safe pure nothrow @nogc
if (is(T == enum))
{
    // instead of slower `std.conv.to`
    switch (value)
    {
        static foreach (member; __traits(allMembers, T)) // instead of slower `EnumMembers`
        {
        case member:
            return __traits(getMember, T, member); // NOTE this is slower: mixin(`return T.` ~ member ~ `;`);
        }
    default:
        return defaultValue;
    }
}

///
@safe pure nothrow /*TODO @nogc*/ unittest
{
    assert("42_1".toDefaulted!int(42) == 42);
    assert(42.toDefaulted!string("_42") == "42");
}

///
@safe pure nothrow @nogc unittest
{
    enum E { unknown, x, y, z, z2 = z, }
    assert("x".toDefaulted!(E)(E.init) == E.x);
    assert("z".toDefaulted!(E)(E.init) == E.z);
    assert("z2".toDefaulted!(E)(E.init) == E.z);
    assert("_".toDefaulted!(E)(E.init) == E.unknown);
}

/** Faster implementation of `std.conv.to` for enumerations with no aliases.
 */
string toStringNonAliases(T)(T value) @safe pure nothrow @nogc
if (is(T == enum))              // TODO check for no aliases
{
    final switch (value)
    {
        static foreach (member; __traits(allMembers, T))
        {
        case __traits(getMember, T, member):
            return member;
        }
    }
}

/** More tolerant variant of `std.conv.to`.
*/
auto tolerantTo(U, S)(S value,
                      bool tryStrippingPluralS = true,
                      bool tryToLower = true,
                      bool tryLevenshtein = true,
                      size_t levenshteinMaxDistance = 3)
if (isSomeString!S)
{
    import std.conv: to;
    try
    {
        return value.to!U;
    }
    catch (Exception e)
    {
        try
        {
            if (tryToLower)
            {
                import std.uni: toLower;
                return value.toLower.tolerantTo!U(tryStrippingPluralS,
                                                  false,
                                                  tryLevenshtein,
                                                  levenshteinMaxDistance);
            }
        }
        catch (Exception e)
        {
            import std.algorithm.searching: endsWith;
            if (tryStrippingPluralS &&
                value.endsWith(`s`))
            {
                try
                {
                    return value[0 .. $ - 1].tolerantTo!U(false,
                                                          tryToLower,
                                                          tryLevenshtein,
                                                          levenshteinMaxDistance);
                }
                catch (Exception e)
                {
                }
            }
        }
    }

    static if (is(U == enum))
    {
        if (tryLevenshtein)
        {
            import std.traits: EnumMembers;
            auto members = [EnumMembers!U]; // TODO make const
            import std.range: empty, front;
            if (!members.empty)
            {
                import std.algorithm.iteration: map;
                import std.algorithm.comparison: levenshteinDistance;
                import std.algorithm.searching: minPos;
                import std.typecons: tuple;
                return members.map!(s => tuple(value.levenshteinDistance(s.to!string), s))
                              .minPos!"a[0] < b[0]".front[1];
            }
        }
    }

    return U.init;
}

@safe /*pure*/ unittest  // TODO make pure when Issue 14962 is fixed
{
    enum E { _, alpha, beta, gamma }

    assert("alpha".tolerantTo!E == E.alpha);
    assert("alphas".tolerantTo!E == E.alpha);
    assert("alph".tolerantTo!E == E.alpha);
    assert("alp".tolerantTo!E == E.alpha);

    assert("gamma".tolerantTo!E == E.gamma);
    assert("gamm".tolerantTo!E == E.gamma);
    assert("gam".tolerantTo!E == E.gamma);

    assert("_".tolerantTo!E == E._);
}

private auto parseError(lazy string msg, string fn = __FILE__, size_t ln = __LINE__) @safe pure
{
    import std.conv : ConvException;
    return new ConvException("Can't parse string: " ~ msg, fn, ln);
}

private void parseCheck(alias source)(dchar c, string fn = __FILE__, size_t ln = __LINE__)
{
    if (source.empty)
        throw parseError(text("unexpected end of input when expecting", "\"", c, "\""));
    if (source.front != c)
        throw parseError(text("\"", c, "\" is missing"), fn, ln);
    import std.range : popFront;
    source.popFront();
}

/**
   Copied this from std.conv.
   TODO Reuse std.conv.parseEscape when moved there.
*/
private dchar parseEscape(Source)(ref Source s)
    if (isSourceOfSomeChar!Source)
{
    import std.range : empty, front, popFront;
    if (s.empty)
        throw parseError("Unterminated escape sequence");

    dchar getHexDigit()(ref Source s_ = s)  // workaround
    {
        import std.ascii : isAlpha, isHexDigit;
        if (s_.empty)
            throw parseError("Unterminated escape sequence");
        import std.range : popFront;
        s_.popFront();
        if (s_.empty)
            throw parseError("Unterminated escape sequence");
        dchar c = s_.front;
        if (!isHexDigit(c))
            throw parseError("Hex digit is missing");
        return isAlpha(c) ? ((c & ~0x20) - ('A' - 10)) : c - '0';
    }

    dchar result;

    switch (s.front)
    {
    case '"':   result = '\"';  break;
    case '\'':  result = '\'';  break;
    case '0':   result = '\0';  break;
    case '?':   result = '\?';  break;
    case '\\':  result = '\\';  break;
    case 'a':   result = '\a';  break;
    case 'b':   result = '\b';  break;
    case 'f':   result = '\f';  break;
    case 'n':   result = '\n';  break;
    case 'r':   result = '\r';  break;
    case 't':   result = '\t';  break;
    case 'v':   result = '\v';  break;
    case 'x':
        result  = getHexDigit() << 4;
        result |= getHexDigit();
        break;
    case 'u':
        result  = getHexDigit() << 12;
        result |= getHexDigit() << 8;
        result |= getHexDigit() << 4;
        result |= getHexDigit();
        break;
    case 'U':
        result  = getHexDigit() << 28;
        result |= getHexDigit() << 24;
        result |= getHexDigit() << 20;
        result |= getHexDigit() << 16;
        result |= getHexDigit() << 12;
        result |= getHexDigit() << 8;
        result |= getHexDigit() << 4;
        result |= getHexDigit();
        break;
    default:
        import std.conv : to;
        throw parseError("Unknown escape character at front of " ~ to!string(s));
    }
    if (s.empty)
        throw parseError("Unterminated escape sequence");

    import std.range : popFront;
    s.popFront();

    return result;
}

/** Parse/Decode Escape Sequences in $(S s) into Unicode Characters $(D dchar).
    Returns: $(D InputRange) of $(D dchar)
    TODO Add to Phobos
 */
auto decodeEscapes(Source)(Source s)
    if (isSourceOfSomeChar!Source)
{
    alias E = ElementType!Source;
    static struct Result
    {
        import std.range : isInfinite;

        this(Source s_)
        {
            _remainingSource = s_;
            popFront;
        }

        // empty
        static if (isInfinite!Source)
            enum bool empty = false;
        else
            @property bool empty() const { return _empty; }

        @property E front() const { return _decodedFront; }

        void popFront()
        {
            import std.range : empty, front, popFront;
            if (!_remainingSource.empty)
            {
                if (_remainingSource.front == '\\') // TODO nothrow
                {
                    _remainingSource.popFront();
                    _decodedFront = _remainingSource.parseEscape;
                }
                else
                {
                    _decodedFront = _remainingSource.front;
                    _remainingSource.popFront();
                }
            }
            else
                _empty = true;
        }

    private:
        Source _remainingSource;
        E _decodedFront;
        static if (!isInfinite!Source)
            bool _empty;
    }

    return Result(s);
}

string decodeEscapesToUTF8(S)(S s)
if (isSourceOfSomeChar!S)
{
    import std.conv : to;
    return s.decodeEscapes.to!(typeof(return));
}

wstring decodeEscapesToUTF16(S)(S s)
if (isSourceOfSomeChar!S)
{
    import std.conv : to;
    return s.decodeEscapes.to!(typeof(return));
}

///
@safe pure /*TODO nothrow*/ unittest
{
    import std.algorithm : equal;
    assert(`s\u00F6der`.decodeEscapes.equal("söder"));
    assert(`\u00F6`.decodeEscapes.equal("ö"));
    assert(`_\u00F6\u00F6_`.decodeEscapes.equal("_öö_"));
    assert(`http://dbpedia.org/resource/Malm\u00F6`.decodeEscapes.equal(`http://dbpedia.org/resource/Malmö`));
    import std.array : array;
    auto y = `_\u00F6\u00F6_`.decodeEscapes.array;
    static assert(is(typeof(y) == dchar[]));
    assert(y == "_öö_");
}

///
@safe pure /*TODO nothrow*/ unittest
{
    import std.algorithm : equal;
    auto y = `_\u00F6\u00F6_`.decodeEscapesToUTF8;
    static assert(is(typeof(y) == string));
    assert(y == "_öö_");
}

///
@safe pure /*TODO nothrow*/ unittest
{
    import std.algorithm : equal;
    auto y = `_\u00F6\u00F6_`.decodeEscapesToUTF16;
    static assert(is(typeof(y) == wstring));
    assert(y == "_öö_");
}

auto unescaped(S)(S s)
if (isSomeString!S)
{
    import std.algorithm.searching : canFind;
    import std.conv : to;
    return (s.canFind('\\') ?
            s.decodeEscapes.to!S :
            s);
}

auto unescape(S)(ref S s)
if (isSomeString!S)
{
    return s = s.unescaped;
}

unittest
{
    import std.algorithm : equal;
    import std.meta : AliasSeq;
    foreach (S; AliasSeq!(string, wstring))
    {
        S x = `_\u00F6\u00F6_`;
        auto y = x.unescaped;
        static assert(is(typeof(y) == S));
        assert(y == "_öö_");

        x.unescape;
        assert(x == "_öö_");
    }
}

// /** Range Implementation of std.utf.toUTF8.
//     Add to Phobos std.utf
// */
// string toUTF8(S)(S s)
// if (isInputRange!S &&
//         is(ElementType!S == dchar))
// {
//     import std.range : isRandomAccessRange;
//     import std.utf : toUTF8;
//     import std.conv : to;
//     static if (isRandomAccessRange!S)
//         return std.utf.toUTF8(s); // reuse array overload
//     else
//         return s.to!(typeof(return));
// }

// /** Range Implementation of std.utf.toUTF16.
//     Add to Phobos std.utf
// */
// wstring toUTF16(S)(S s)
// if (isInputRange!S &&
//         is(ElementType!S == dchar))
// {
//     import std.range : isRandomAccessRange;
//     import std.utf : toUTF16;
//     import std.conv : to;
//     static if (isRandomAccessRange!S)
//         return std.utf.toUTF16(s); // reuse array overload
//     else
//         return s.to!(typeof(return));
// }
