/** SUO-KIF File Format. */
module suo_kif;

/** Parse SUO-KIF from `source`. */
void parseSUOKIF(R)(R source)
{
    import std.range : empty, front, popFront;
    import std.uni : isWhite;
    import std.algorithm : among;
    import dbgio;

    /// Skip comment.
    void skipComment()
    {
        dln();
        while (!source.empty &&
               !source.front.among('\r', '\n')) // until end of line
        {
            source.popFront();
        }
    }

    /// Skip whitespace.
    void skipWhite()
    {
        dln();
        while (!source.empty &&
               !source.front.isWhite) // until end of line
        {
            source.popFront();
        }
    }

    while (!source.empty)
    {
        dln();
        if (source.front == ';') { skipComment(); }
        else if (source.front.isWhite) { skipWhite(); }
        else
        {
            assert(false);
        }
    }
}

unittest
{
    import std.path : expandTilde;
    import std.file : readText;
    readText("~/Work/phobos-next/src/emotion.kif".expandTilde).parseSUOKIF();
}
