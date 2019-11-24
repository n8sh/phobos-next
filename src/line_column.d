module line_column;

@safe pure nothrow @nogc:

struct LineColumn
{
    size_t line;
    size_t column;
}

LineColumn offsetLineColumn(scope const char[] haystack,
                            size_t offset)
{
    // find 0-based column offset
    size_t cursor = offset;      // cursor
    while (cursor != 0 &&        // TODO extend to support UTF-8
           !(haystack[cursor] == '\n' ||
             haystack[cursor] == '\r'))
    {
        cursor -= 1;
    }
    const column = offset-cursor;

    // find 0-based line offset
    size_t lineCounter = 0;
    while (cursor != 0)
    {
        if (haystack[cursor] == '\n' ||
            haystack[cursor] == '\r')
        {
            cursor -= 1;
            if (cursor != 0 &&
                (haystack[cursor] == '\r')) // DOS-style line ending "\r\n"
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
