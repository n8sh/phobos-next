module nxt.line_column;

@safe:

void diagnoseGNUStyle(Args...)(scope const string tag,
                               scope const string path,
                               const LineColumn lc,
                               Args args)
{
    import std.stdio : writeln;
    debug writeln(path,
                  ":", lc.line + 1, // line offset starts at 1
                  ":", lc.column,   // column counter starts at 0
                  ": ", tag, ": ", args);
}

@safe pure nothrow @nogc:

struct LineColumn
{
    size_t line;
    size_t column;
}

/** Convert `offset` at `haystack` to line and column. */
LineColumn offsetLineColumn(scope const char[] haystack,
                            size_t offset)
{
    // find 0-based column offset
    size_t cursor = offset;      // cursor
    while (cursor != 0) // TODO extend to support UTF-8
    {
        if (cursor >= 1)
        {
            if (haystack[cursor - 1] == '\n' || // TODO extend to support UTF-8
                haystack[cursor - 1] == '\r')   // TODO extend to support UTF-8
            {
                break;
            }
        }
        cursor -= 1;
    }
    // cursor is not at beginning of line

    const column = offset-cursor; // column byte offset

    // find 0-based line offset
    size_t lineCounter = 0;
    while (cursor != 0)
    {
        if (haystack[cursor - 1] == '\n' ||
            haystack[cursor - 1] == '\r')
        {
            cursor -= 1;
            if (cursor != 0 &&
                (haystack[cursor - 1] == '\r')) // DOS-style line ending "\r\n"
            {
                cursor -= 1;
            }
            else            // UNIX-style line ending "\n"
            {
            }
            lineCounter += 1;
        }
        else                // no line ending at cursor
        {
            cursor -= 1;
        }
    }

    return typeof(return)(lineCounter, column);
}

@safe pure unittest
{
    auto x = "\nx\n y";
    assert(x.length == 5);
    assert(x.offsetLineColumn(0) == LineColumn(0, 0));
    assert(x.offsetLineColumn(1) == LineColumn(1, 0));
    assert(x.offsetLineColumn(2) == LineColumn(1, 1));
    assert(x.offsetLineColumn(3) == LineColumn(2, 0));
    assert(x.offsetLineColumn(4) == LineColumn(2, 1));
}

version(unittest)
{
    import nxt.dbgio;
}
