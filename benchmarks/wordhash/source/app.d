void main()
{
    import std.stdio;
    foreach (const line; File("/usr/share/dict/words").byLine)
    {
        if (line.length >= 3 &&
            line[$ - 2 .. $] != `'s`)
        {
            writeln(line);
        }
    }
}
