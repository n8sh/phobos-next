module capitalization;

import std.traits : isSomeString;

/** Check if `s` starts with a capital letter followed by a lower
    letter. */
bool isCapitalizedEasy(S)(S s)
    if (isSomeString!S)
{
    import std.range.primitives : empty, front, popFront;
    import std.uni : isUpper, isLower;

    if (s.empty) { return false; }

    const firstUpper = s.front.isUpper;
    if (!firstUpper) return false;
    s.popFront();

    if (s.empty) { return false; }
    return s.front.isLower;
}

@safe pure unittest
{
    assert(!`A`.isCapitalizedEasy);
    assert(!`a`.isCapitalizedEasy);
    assert(!`alpha`.isCapitalizedEasy);
    assert(!`ALPHA`.isCapitalizedEasy);
    assert(!`aThing`.isCapitalizedEasy);
    assert(`Alpha`.isCapitalizedEasy);
    assert(`Jack London`.isCapitalizedEasy);
}

import std.uni : isLower;
auto isLowercased(S, alias pred = isLower)(S s)
    if (isSomeString!S)
{
    import std.algorithm.searching : all;
    import std.traits : isNarrowString;
    import std.utf : byUTF;
    // TODO functionize
    static if (isNarrowString!S) return s.byUTF!dchar.all!(ch => pred(ch));
    else                         return t.map!(ch => pred(ch));
}

@safe pure unittest
{
    assert(!`A`.isLowercased);
    assert(`a`.isLowercased);
}

import std.uni : isUpper;
auto isUppercased(S, alias pred = isUpper)(S s)
    if (isSomeString!S)
{
    import std.algorithm.searching : all;
    import std.traits : isNarrowString;
    import std.utf : byUTF;
    // TODO functionize
    static if (isNarrowString!S) return s.byUTF!dchar.all!(ch => pred(ch));
    else                         return t.map!(ch => pred(ch));
}

@safe pure unittest
{
    assert(`A`.isUppercased);
    assert(!`a`.isUppercased);
}

/** Check if `s` starts with a capital letter followed by only lower
    letters. */
bool isCapitalized(S)(S s)
    if (isSomeString!S)
{
    import std.range.primitives : empty, front, popFront;

    if (s.empty) { return false; }

    import std.ascii : isDigit;
    import std.uni : isUpper;
    const firstDigit = s.front.isDigit;
    const firstUpper = s.front.isUpper;

    if (!(firstDigit ||
          firstUpper))
        return false;

    s.popFront();

    if (s.empty)
    {
        return firstDigit;
    }
    else
    {
        import std.uni : isLower;
        import std.algorithm.searching : all;
        return s.all!(x => (x.isDigit ||
                            x.isLower));
    }
}

@safe pure unittest
{
    assert(!``.isCapitalized);
    assert(!`alpha`.isCapitalized);
    assert(!`ALPHA`.isCapitalized);
    assert(!`aThing`.isCapitalized);
    assert(`Alpha`.isCapitalized);
    assert(!`Jack London`.isCapitalized);
}

/** Return `true` if `s` has proper name-style capitalization, such as in `Alpha
 * Centauri`.
 */
bool isNameCapitalized(S)(S s)
    if (isSomeString!S)
{
    import std.algorithm.iteration : splitter;
    import std.algorithm.searching : all;
    import std.algorithm.comparison : among;
    import std.ascii : isWhite;
    import std.range : enumerate;
    import std.uni : isUpper;
    return s.splitter!(s => (s.isWhite || s.among!('-') != 0))
            .enumerate
            .all!(x => ((x.index >= 1 &&
                         (x.value.all!(x => x.isUpper) || // Henry II
                          x.value.among!(`of`, `upon`))) ||
                        x.value.isCapitalized)); // TODO add enumerate and all middle word to be a preposition
}

@safe pure unittest
{
    assert(!`alpha`.isNameCapitalized);
    assert(!`alpha centauri`.isNameCapitalized);
    assert(!`ALPHA`.isNameCapitalized);
    assert(!`ALPHA CENTAURI`.isNameCapitalized);
    assert(!`aThing`.isNameCapitalized);
    assert(`Alpha`.isNameCapitalized);
    assert(`Alpha Centauri`.isNameCapitalized);
    assert(`11104 Airion`.isNameCapitalized);
    assert(`New York City`.isNameCapitalized);
    assert(`1-Hexanol`.isNameCapitalized);
    assert(`11-Hexanol`.isNameCapitalized);
    assert(`22nd Army`.isNameCapitalized);
    assert(!`22nd army`.isNameCapitalized);
    assert(`2nd World War`.isNameCapitalized);
    assert(`Second World War`.isNameCapitalized);
    assert(`Värmland`.isNameCapitalized);
    assert(!`The big sky`.isNameCapitalized);
    assert(`Suur-London`.isNameCapitalized);
    assert(`Kingdom of Sweden`.isNameCapitalized);
    assert(`Stratford upon Avon`.isNameCapitalized);
    assert(`Henry II`.isNameCapitalized);
}
