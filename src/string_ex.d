module string_ex;

import traits_ex : isCharsSlice;

Chars[] quotedWords(Chars)(Chars s,
                           const scope string quoteBeginChar = `"`,
                           const scope string quoteEndChar = `"`)
if (isCharsSlice!Chars)
{
    typeof(return) words;
    import std.array : array;
    import splitter_ex : splitterASCIIAmong;
    import std.algorithm : filter;
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

            words ~= s[0 .. currI].splitterASCIIAmong!(' ')
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
            words ~= s.splitterASCIIAmong!(' ')
                      .filter!(a => !a.empty)
                      .array;
            s = [];
        }
    }
    return words;
}

///
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
