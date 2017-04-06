module string_ex;

import std.traits : isSomeString;

/** Check if $(D s) starts with a capital letter followed by a lower
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

/** Check if $(D s) starts with a capital letter followed by only lower
    letters. */
bool isCapitalized(S)(S s)
    if (isSomeString!S)
{
    import std.range.primitives : empty, front, popFront;
    import std.ascii : isDigit;
    import std.uni : isUpper, isLower;

    if (s.empty) { return false; }

    const firstDigit = s.front.isDigit;
    const firstUpper = s.front.isUpper;
    if (!(firstDigit ||
          firstUpper))
        return false;

    s.popFront();

    import std.algorithm.searching : all;

    if (s.empty)
        return firstDigit;
    else
        return s.all!(x => (x.isDigit ||
                            x.isLower));
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

/** Return `true` if `s` has proper name-style capitalization, such as in `Alpha Centauri`.
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

S[] quotedWords(S)(S s,
                   string quoteBeginChar = `"`,
                   string quoteEndChar = `"`)
    if (isSomeString!S)
{
    typeof(return) words;
    import std.array : array;
    import std.algorithm : filter, splitter;
    import std.string : indexOf, lastIndexOf;
    import std.range : empty;
    while (!s.empty)
    {
        auto quoteBeginI = s.indexOf(quoteBeginChar);
        if (quoteBeginI >= 0)
        {
            auto currI = quoteBeginI;

            auto prefixBeginI = s[0 .. currI].lastIndexOf(' ');
            if (prefixBeginI >= 0)
            {
                currI = prefixBeginI + 1;
            }

            words ~= s[0 .. currI].splitter(' ')
                                  .filter!(a => !a.empty)
                                  .array;

            auto quoteEndI = s[quoteBeginI + 1 .. $].indexOf(quoteEndChar) + quoteBeginI + 1;
            auto suffixEndI = s[quoteEndI + 1 .. $].indexOf(' ');
            if (suffixEndI >= 0)
            {
                quoteEndI = quoteEndI + suffixEndI;
            }
            words ~= s[currI .. quoteEndI + 1];
            s = s[quoteEndI + 1 .. $];
        }
        else
        {
            words ~= s.splitter(' ')
                      .filter!(a => !a.empty)
                      .array;
            s = [];
        }
    }
    return words;
}

@safe pure unittest
{
    import std.stdio;
    import std.algorithm.comparison : equal;
    const t = `verb:is   noun:"New York" a noun:"big  city"@en `;
    const x = t.quotedWords;
    const xs = [`verb:is`, `noun:"New York"`, `a`, `noun:"big  city"@en`];
    assert(equal(x, xs));
    // TODO assert(equal(` verb:is   name:"New York"@en article:a noun:"big  city"@en `.quotedWords,
    //              [`verb:is`, `name:"New York"@en`, `article:a`, `noun:"big  city"@en`]));
}

/** Check if `s` contains more than one word. */
auto isMultiWord(const(char)[] s)
{
    import std.algorithm.searching : canFind;
    return s.canFind(`_`, ` `) >= 1;
}

@safe pure unittest
{
    assert(isMultiWord("hey there"));
}
