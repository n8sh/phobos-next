module find_split_ex;

import std.traits : isExpressions;

/** Like `findSplit` but with multiple separator `needles` known at compile-time
 * to prevent `NarrowString` decoding.
 */
template findSplitAmong(needles...)
    if (isExpressions!needles)  // all needs
{
    import std.meta : staticMap;
    import std.traits : Unqual;
    import traits_ex : allSameTypeIterative;

    auto findSplitAmong(Haystack)(scope return Haystack haystack)
        if (is(typeof(Haystack.init[0 .. 0])) && // can be sliced
            // TODO allCompareable to Haystack element except for `NarrowStrings`
            allSameTypeIterative!(Unqual!(typeof(Haystack.init[0])),
                                  staticMap!(Unqual, typeof(needles))))
    {
        // same as in `std.algorithm.searching.findSplit`
        static struct Result
        {
            private Haystack[3] _tuple;
            alias _tuple this;

            @property inout:
            ref inout(Haystack) pre() { return _tuple[0]; };
            ref inout(Haystack) separator() { return _tuple[1]; };
            ref inout(Haystack) post() { return _tuple[2]; };

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
                return Result([haystack[0 .. offset],
                               haystack[offset .. offset + 1],
                               haystack[offset + 1 .. $]]);
            }
        }
        return Result([haystack, [], []]);
    }
}

///
@safe pure nothrow @nogc unittest
{
    auto r1 = "a+b*c".findSplitAmong!('+', '-');
    assert(r1);
    assert(r1.pre == "a");
    assert(r1.separator == "+");
    assert(r1.post == "b*c");

    const r2 = "a+b*c".findSplitAmong!('-', '*');
    assert(r2);
    assert(r2.pre == "a+b");
    assert(r2.separator == "*");
    assert(r2.post == "c");

    immutable r3 = "a+b*c".findSplitAmong!('/');
    assert(!r3);
    assert(r3.pre == "a+b*c");
    assert(r3.separator == []);
    assert(r3.post == []);
}
