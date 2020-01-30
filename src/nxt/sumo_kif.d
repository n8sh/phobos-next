/** Helper functions for the SU(M)O-KIF file format.
 *
 * SUO-KIF is used the default encoding of the SUMO ontology.
 */
module nxt.suo_kif;

@safe:

bool isFormat(scope const(char)[] chars) pure nothrow @nogc
{
    import nxt.array_algorithm : findSkip;
    if (chars.findSkip('%'))
    {
        if (chars.length >= 1)
        {
            import std.ascii: isDigit;
            return isDigit(chars[0]) || chars[0] == '*';
        }
    }
    return false;
}

@safe pure unittest
{
    assert("%1".isFormat);
    assert(" %1 ".isFormat);

    assert("%2".isFormat);
    assert(" %2 ".isFormat);

    assert("%*".isFormat);
    assert(" %* ".isFormat);

    assert(!"%".isFormat);
    assert(!"% ".isFormat);
    assert(!" % ".isFormat);
}

bool isTermFormat(scope const(char)[] chars) pure nothrow @nogc
{
    return !isFormat(chars);
}
