module find_ex;

import std.typecons: Tuple, tuple;
import std.string: CaseSensitive;
import std.functional: binaryFun;

enum FindContext { inWord, inSymbol,
                   asWord, asSymbol }

/** Return true if $(D a) is a C-style Identifier symbol character. */
bool isSymbol(T)(in T a)
    @safe pure nothrow @nogc
{
    import std.ascii: isAlpha;
    return a.isAlpha || a == '_';
}

bool isSymbolASCII(string rest, ptrdiff_t off, size_t end)
    @safe pure nothrow @nogc
in
{
    assert(end <= rest.length);
}
body
{
    import std.ascii: isAlphaNum;
    return ((off == 0 || // either beginning of line
             !rest[off - 1].isAlphaNum &&
             rest[off - 1] != '_') &&
            (end == rest.length || // either end of line
             !rest[end].isAlphaNum &&
             rest[end] != '_'));
}

///
@safe pure nothrow @nogc unittest
{
    assert(isSymbolASCII("alpha", 0, 5));
    assert(isSymbolASCII(" alpha ", 1, 6));
    assert(!isSymbolASCII("driver", 0, 5));
    assert(!isSymbolASCII("a_word", 0, 1));
    assert(!isSymbolASCII("first_a_word", 6, 7));
}

bool isWordASCII(string rest, ptrdiff_t off, size_t end)
    @safe pure nothrow @nogc
in
{
    assert(end <= rest.length);
}
body
{
    import std.ascii: isAlphaNum;
    return ((off == 0 || // either beginning of line
             !rest[off - 1].isAlphaNum) &&
            (end == rest.length || // either end of line
             !rest[end].isAlphaNum));
}

///
@safe pure nothrow @nogc unittest
{
    assert(isSymbolASCII("alpha", 0, 5));
    assert(isSymbolASCII(" alpha ", 1, 6));
    assert(!isSymbolASCII("driver", 0, 5));
    assert(isWordASCII("a_word", 0, 1));
    assert(isWordASCII("first_a_word", 6, 7));
    assert(isWordASCII("first_a", 6, 7));
}

// Parameterize on isAlpha and isSymbol.

/** Find $(D needle) as Word or Symbol Acronym at $(D haystackOffset) in $(D haystack).
    TODO Make it compatible (specialized) for InputRange or BidirectionalRange.
*/
Tuple!(R, ptrdiff_t[]) findAcronymAt(alias pred = "a == b",
                                     R,
                                     E)(R haystack,
                                        E needle,
                                        FindContext ctx = FindContext.inWord,
                                        CaseSensitive cs = CaseSensitive.yes, // TODO Use this
                                        size_t haystackOffset = 0) @safe pure
{
    import std.ascii: isAlpha;
    import std.algorithm: find;
    import std.range: empty;

    scope auto aOffs = new ptrdiff_t[needle.length]; // acronym hit offsets

    auto rest = haystack[haystackOffset..$];
    while (needle.length <= rest.length) // for each new try at finding the needle at remainding part of haystack
    {
        /* debug dln(needle, ", ", rest); */

        // find first character
        size_t nIx = 0;         // needle index
        rest = rest.find!pred(needle[nIx]); // reuse std.algorithm: find!
        if (rest.empty) { return tuple(rest, ptrdiff_t[].init); } // degenerate case
        aOffs[nIx++] = &rest[0] - &haystack[0]; // store hit offset and advance acronym
        rest = rest[1 .. $];
        const ix0 = aOffs[0];

        // check context before point
        final switch (ctx)
        {
            case FindContext.inWord:   break; // TODO find word characters before point and set start offset
            case FindContext.inSymbol: break; // TODO find symbol characters before point and set start offset
            case FindContext.asWord:
                if (ix0 >= 1 && haystack[ix0-1].isAlpha) { goto miss; } // quit if not word start
                break;
            case FindContext.asSymbol:
                if (ix0 >= 1 && haystack[ix0-1].isSymbol) { goto miss; } // quit if not symbol stat
                break;
        }

        while (rest)            // while elements left in haystack
        {

            // Check elements in between
            ptrdiff_t hit = -1;
            import std.algorithm: countUntil;
            final switch (ctx)
            {
                case FindContext.inWord:
                case FindContext.asWord:
                    hit = rest.countUntil!(x => (binaryFun!pred(x, needle[nIx])) || !x.isAlpha); break;
                case FindContext.inSymbol:
                case FindContext.asSymbol:
                    hit = rest.countUntil!(x => (binaryFun!pred(x, needle[nIx])) || !x.isSymbol); break;
            }
            if (hit == -1) { goto miss; } // no hit this time

            // Check if hit
            if (hit == rest.length || // if we searched till the end
                rest[hit] != needle[nIx]) // acronym letter not found
            {
                rest = haystack[aOffs[0]+1 .. $]; // try beyond hit
                goto miss;      // no hit this time
            }

            aOffs[nIx++] = (&rest[0] - &haystack[0]) + hit; // store hit offset and advance acronym
            if (nIx == needle.length) // if complete acronym found
            {
                return tuple(haystack[aOffs[0] .. aOffs[$-1] + 1], aOffs) ; // return its length
            }
            rest = rest[hit+1 .. $]; // advance in source beyound hit
        }
    miss:
        continue;
    }
    return tuple(R.init, ptrdiff_t[].init); // no hit
}

///
@safe pure unittest
{
    assert("size_t".findAcronymAt("sz_t", FindContext.inWord)[0] == "size_t");
    assert("size_t".findAcronymAt("sz_t", FindContext.inSymbol)[0] == "size_t");
    assert("åäö_ab".findAcronymAt("ab")[0] == "ab");
    assert("fopen".findAcronymAt("fpn")[0] == "fopen");
    assert("fopen_".findAcronymAt("fpn")[0] == "fopen");
    assert("_fopen".findAcronymAt("fpn", FindContext.inWord)[0] == "fopen");
    assert("_fopen".findAcronymAt("fpn", FindContext.inSymbol)[0] == "fopen");
    assert("f_open".findAcronymAt("fpn", FindContext.inWord)[0] == []);
    assert("f_open".findAcronymAt("fpn", FindContext.inSymbol)[0] == "f_open");
}

import std.traits : isExpressions;
import traits_ex : allSameTypeIterative;

/** Like `findSplit` but with multiple `needles` known at compile-time to
 * prevent `NarrowString` decoding.
 */
template findSplitN(needles...)
    if (isExpressions!needles)
{
    import std.meta : staticMap;
    import std.traits : Unqual;

    auto findSplitN(Haystack)(scope return Haystack haystack)
        if (is(typeof(Haystack.init[0 .. 0])) && // can be sliced
            allSameTypeIterative!(Unqual!(typeof(Haystack.init[0])),
                                  staticMap!(Unqual, typeof(needles))))
    {
        import std.algorithm.searching : findSplit;
        static struct Result
        {
            Haystack pre;
            Haystack separator;
            Haystack post;
            bool opCast(T : bool)() const
            {
                import std.range : empty;
                return !separator.empty;
            }
        }
        foreach (immutable offset; 0 .. haystack.length)
        {
            import std.algorithm.comparison : among;
            if (const uint hitIndex = haystack[offset].among!(needles))
            {
                return Result(haystack[0 .. offset],
                              haystack[offset .. offset + 1],
                              haystack[offset + 1 .. $]);
            }
        }
        return Result(haystack, [], []);
    }
}

@safe pure nothrow @nogc unittest
{
    auto r1 = "a+b*c".findSplitN!('+', '-');
    assert(r1);
    assert(r1.pre == "a");
    assert(r1.separator == "+");
    assert(r1.post == "b*c");

    const r2 = "a+b*c".findSplitN!('-', '*');
    assert(r2);
    assert(r2.pre == "a+b");
    assert(r2.separator == "*");
    assert(r2.post == "c");

    immutable r3 = "a+b*c".findSplitN!('/');
    assert(!r3);
    assert(r3.pre == "a+b*c");
    assert(r3.separator == []);
    assert(r3.post == []);
}

version(unittest)
{
    import std.algorithm.comparison : equal;
    import array_help : s;
}
