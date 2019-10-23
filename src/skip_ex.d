/** Extensions to skipOver.
 *
 * See_Also: https://forum.dlang.org/post/tswdobtabsjarszfkmbt@forum.dlang.org
 */
module skip_ex;

import std.functional : binaryFun;
import std.range.primitives : front, back, save, empty, popBack, hasSlicing, isBidirectionalRange, ElementType;

version(unittest)
{
    import array_help : s;
}

/** Skip over the ending portion of `r1` that matches `r2`, or nothing upon no match.
 *
 * See_Also: std.algorithm.searching.skipOver.
 */
bool skipOverBack(R1, R2)(scope ref R1 r1,
                          scope R2 r2)
if (isBidirectionalRange!R1 &&
    isBidirectionalRange!R2 &&
    is(typeof(r1.back == r2.back)))
{
    static if (is(typeof(r1[0 .. $] == r2) : bool) &&
               is(typeof(r2.length > r1.length) : bool) &&
               is(typeof(r1 = r1[0 .. $ - r2.length])))
    {
        if (r1.length >= r2.length &&
            r1[$ - r2.length .. $] == r2)
        {
            r1 = r1[0 .. r1.length - r2.length];
            return true;
        }
        return false;
    }
    else
    {
        return skipOverBack!((a, b) => a == b)(r1, r2);
    }
}

///
bool skipOverBack(alias pred, R1, R2)(scope ref R1 r1,
                                      scope R2 r2)
if (isBidirectionalRange!R1 &&
    isBidirectionalRange!R2 &&
    is(typeof(binaryFun!pred(r1.back, r2.back)))) // TODO R2 doesn't have to bi-directional if R1 is RandomAccess and R2.hasLength
{
    import std.range.primitives : hasLength;
    static if (hasLength!R1 && hasLength!R2)
    {
        // Shortcut opportunity!
        if (r2.length > r1.length)
            return false;
    }
    auto r = r1.save;
    while (!r2.empty &&
           !r.empty &&
           binaryFun!pred(r.back, r2.back))
    {
        r.popBack();
        r2.popBack();
    }
    if (r2.empty)
    {
        r1 = r;
    }
    return r2.empty;
}

///
@safe pure nothrow @nogc unittest
{
    auto s1_ = [1, 2, 3].s;
    auto s1 = s1_[];
    const s2_ = [2, 3].s;
    const s2 = s2_[];
    s1.skipOverBack(s2);
    assert(s1 == [1].s);
    s1.skipOverBack(s2);        // no effect
    assert(s1 == [1].s);
}

@safe pure nothrow @nogc unittest
{
    import std.algorithm : equal;
    auto s1 = "Hello world";
    assert(!skipOverBack(s1, "Ha"));
    assert(s1 == "Hello world");
    assert(skipOverBack(s1, "world") && s1 == "Hello ");
}

/** Is `true` iff all `Ts` are slices with same unqualified matching element types.
 */
template isComparableSlices(Ts...)
if (Ts.length >= 2)
{
    private enum isSlice(T) = is(T : const(E)[], E);
    private enum isSliceOf(T, e) = is(T : const(E)[]);
    static if (isSlice!(Ts[0]))
    {
        alias E = typeof(Ts[0].init[0]);
        static foreach (T; Ts[1 .. $])
        {
            static if (is(typeof(isComparableSlices) == void) && // not yet defined
                       !(isSliceOf!(T, E)))
            {
                enum isComparableSlices = false;
            }
        }
        static if (is(typeof(isComparableSlices) == void)) // if not yet defined
        {
            enum isComparableSlices = true;
        }
    }
    else
    {
        enum isComparableSlices = false;
    }
}

///
@safe pure unittest
{
    static assert(isComparableSlices!(int[], int[]));
    static assert(isComparableSlices!(const(int)[], int[]));
    static assert(isComparableSlices!(int[], const(int)[]));

    static assert(isComparableSlices!(int[], int[], int[]));
    static assert(isComparableSlices!(int[], const(int)[], int[]));
    static assert(isComparableSlices!(const(int)[], const(int)[], const(int)[]));

    static assert(!isComparableSlices!(int, char));
    static assert(!isComparableSlices!(int, int));

    static assert(!isComparableSlices!(int[], char[]));
    static assert(!isComparableSlices!(int[], char[], char[]));
    static assert(!isComparableSlices!(char[], int[]));
}

/** Variadic version of $(D skipOver).
 *
 * Returns: index + 1 into matching $(D needles), 0 otherwise.
 */
size_t skipOverEither(alias pred = "a == b", Range, Ranges...)(scope ref Range haystack,
                                                               scope Ranges needles)
if (Ranges.length >= 2)
{
    import core.internal.traits : allSatisfy;
    import traits_ex : isCharsSlice;
    foreach (const ix, needle; needles)
    {
        static if (pred == "a == b" &&
                   isCharsSlice!(Range) &&
                   allSatisfy!(isCharsSlice, Ranges))
        {
            // `nothrow` char[] fast path
            if (haystack.length >= needle.length &&
                haystack[0 .. needle.length] == needle) // TODO `haystack.ptr`
            {
                haystack = haystack[needle.length .. haystack.length]; // TODO `haystack.ptr`
                return ix + 1;
            }
        }
        else
        {
            import std.algorithm.searching : skipOver;
            if (haystack.skipOver(needle)) // TODO nothrow
            {
                return ix + 1;
            }
        }
    }
    return 0;
}

@safe pure nothrow /* TODO nothrow @nogc */ unittest
{
    import std.algorithm.searching : startsWith;
    auto x = "beta version";
    assert(x.startsWith("beta"));
}

@safe pure /* TODO nothrow @nogc */ unittest
{
    import std.algorithm.searching : skipOver;
    auto x = "beta version";
    assert(x.skipOver("beta"));
    assert(x == " version");
}

@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.skipOverEither("beta", "be") == 1);
    assert(x == " version");
}

@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.skipOverEither("be", "_") == 1);
}

@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.skipOverEither("x", "y") == 0);
}

@safe pure nothrow @nogc unittest
{

    auto x = "beta version";
    assert(x.skipOverEither("x", "y") == 0);
}

/** Skip Over Shortest Matching prefix in $(D needles) that prefixes $(D haystack).
 *
 * TODO Make return value a specific type that has bool conversion so we can
 * call it as
 * if (auto hit = r.skipOverShortestOf(...)) { ... }
 */
size_t skipOverShortestOf(alias pred = "a == b",
                          Range,
                          Ranges...)(scope ref Range haystack,
                                     scope Ranges needles)
if (Ranges.length >= 2)
{
    import std.algorithm : startsWith;
    const hit = startsWith!pred(haystack, needles);
    if (hit)
    {
        // get needle lengths
        size_t[needles.length] lengths;
        foreach (ix, needle; needles)
        {
            import std.traits : isSomeString, isSomeChar;
            import std.range.primitives : ElementType;
            import core.internal.traits : Unqual;

            alias Needle = Unqual!(typeof(needle));

            static if (is(Unqual!Range ==
                          Needle))
            {
                lengths[ix] = needle.length;
            }
            else static if (is(Unqual!(ElementType!Range) ==
                               Unqual!(ElementType!Needle)))
            {
                lengths[ix] = needle.length;
            }
            else static if (isSomeString!Range &&
                            isSomeString!Needle)
            {
                lengths[ix] = needle.length;
            }
            else static if (isSomeChar!(ElementType!Range) &&
                            isSomeChar!Needle)
            {
                lengths[ix] = 1;
            }
            else static if (is(Unqual!(ElementType!Range) ==
                               Needle))
            {
                lengths[ix] = 1;
            }
            else
            {
                static assert(0,
                              "Cannot handle needle of type " ~ Needle.stringof ~
                              " when haystack has ElementType " ~ (ElementType!Range).stringof);
            }
        }

        import std.range: popFrontN;
        haystack.popFrontN(lengths[hit - 1]);
    }

    return hit;

}

@safe pure unittest
{
    auto x = "beta version";
    assert(x.skipOverShortestOf("beta", "be") == 2);
    assert(x == "ta version");
}

@safe pure unittest
{
    auto x = "beta version";
    assert(x.skipOverShortestOf("be", "beta") == 1);
    assert(x == "ta version");
}

@safe pure unittest
{
    auto x = "beta version";
    assert(x.skipOverShortestOf('b', "be", "beta") == 1);
    assert(x == "eta version");
}

/** Skip Over Longest Matching prefix in $(D needles) that prefixes $(D haystack).
 */
SkipOverLongest skipOverLongestOf(alias pred = "a == b", Range, Ranges...)(scope ref Range haystack,
                                                                           scope Ranges needles)
{
    // TODO figure out which needles that are prefixes of other needles by first
    // sorting them and then use some adjacent filtering algorithm
    static assert(0, "TODO implement");
}

/** Skip Over Back Shortest Match of `needles` in `haystack`. */
size_t skipOverBackShortestOf(alias pred = "a == b", Range, Ranges...)(scope ref Range haystack,
                                                                       scope Ranges needles) @trusted
// TODO We cannot prove that cast(ubyte[]) of a type that have no directions is safe
if (Ranges.length >= 2)
{
    import std.range: retro, ElementType;
    import std.traits: hasIndirections;
    import core.internal.traits : Unqual;
    import std.meta: staticMap, AliasSeq;
    // import traits_ex: allSame;

    static if ((!hasIndirections!(ElementType!Range))/*  && */
               /* allSame!(Unqual!Range, staticMap!(Unqual, Ranges)) */)
    {
        auto retroHaystack = (cast(ubyte[])haystack).retro;

        alias Retro(Range) = typeof((ubyte[]).init.retro);
        AliasSeq!(staticMap!(Retro, Ranges)) retroNeedles;
        foreach (ix, needle; needles)
        {
            retroNeedles[ix] = (cast(ubyte[])needle).retro;
        }

        const retroHit = retroHaystack.skipOverShortestOf(retroNeedles);
        haystack = haystack[0.. $ - (haystack.length - retroHaystack.length)];

        return retroHit;
    }
    else
    {
        static assert(0, "Unsupported combination of haystack type " ~ Range.stringof ~
                      " with needle types " ~ Ranges.stringof);
    }
}

@safe pure nothrow @nogc unittest
{
    auto x = "alpha_beta";
    assert(x.skipOverBackShortestOf("x", "beta") == 2);
    assert(x == "alpha_");
}

@safe pure nothrow @nogc unittest
{
    auto x = "alpha_beta";
    assert(x.skipOverBackShortestOf("a", "beta") == 1);
    assert(x == "alpha_bet");
}

/** Drop $(D prefixes) in $(D s).
 *
 * TODO Use multi-argument skipOver when it becomes available
 * http://forum.dlang.org/thread/bug-12335-3@https.d.puremagic.com%2Fissues%2F
*/
void skipOverPrefixes(R, A)(scope ref R s,
                            const scope A prefixes)
{
    import std.algorithm.searching : skipOver;
    foreach (prefix; prefixes)
    {
        if (s.length > prefix.length &&
            s.skipOver(prefix))
        {
            break;
        }
    }
}

/** Drop $(D suffixes) in $(D s).
 */
void skipOverSuffixes(R, A)(scope ref R s,
                            const scope A suffixes)
{
    foreach (suffix; suffixes)
    {
        if (s.length > suffix.length &&
            s.endsWith(suffix))
        {
            s = s[0 .. $ - suffix.length]; // TODO .ptr
            break;
        }
    }
}

/** Drop either both prefix `frontPrefix` and suffix `backSuffix` or do nothing.
 *
 * Returns: `true` upon drop, `false` otherwise.
 */
bool skipOverFrontAndBack(alias pred = "a == b", R, E, F)(scope ref R r,
                                                          scope E frontPrefix,
                                                          scope F backSuffix)
if (isBidirectionalRange!R &&
    is(typeof(binaryFun!pred(ElementType!R.init, E.init))) &&
    is(typeof(binaryFun!pred(ElementType!R.init, F.init))))
{
    import core.internal.traits : Unqual;
    import std.traits : isArray;
    static if (isArray!R &&
               is(Unqual!(typeof(R.init[0])) == E)) // for instance if `R` is `string` and `E` is `char`
    {
        if (r.length >= 2 &&
            r[0] == frontPrefix &&
            r[$ - 1] == backSuffix)
        {
            r = r[1 .. $ - 1];
            return true;
        }
    }
    else
    {
        if (r.length >= 2 && // TODO express this requirement in `r` as `hasLength`
            binaryFun!pred(r.front, frontPrefix) &&
            binaryFun!pred(r.back, backSuffix))
        {
            import std.range.primitives : popBack, popFront;
            r.popFront();
            r.popBack();
            return true;
        }
    }
    return false;
}

@safe pure nothrow @nogc unittest
{
    auto expr = `"alpha"`;
    assert(expr.skipOverFrontAndBack('"', '"'));
    assert(expr == `alpha`);
}

@safe pure nothrow @nogc unittest
{
    auto expr_ = `"alpha"`;
    auto expr = expr_;
    assert(!expr.skipOverFrontAndBack(',', '"'));
    assert(expr == expr_);
}

@safe pure nothrow @nogc unittest
{
    auto expr_ = `"alpha`;
    auto expr = expr_;
    assert(!expr.skipOverFrontAndBack('"', '"'));
    assert(expr == expr_);
}

@safe pure nothrow @nogc unittest
{
    auto expr_ = `alpha"`;
    auto expr = expr_;
    assert(!expr.skipOverFrontAndBack('"', '"'));
    assert(expr == expr_);
}
