void main()
{
    import std.stdio;

    import hashset : HashSet;
    import hashmap : HashMap;
    import basic_array : BasicArray;

    alias Ix = size_t;
    alias Str = BasicArray!char;
    alias Strs = HashSet!(Str);
    alias IxStr = HashMap!(Ix, Str);

    foreach (const line; File("/usr/share/dict/words").byLine)
    {
        if (line.length >= 3 &&
            line[$ - 2 .. $] != `'s`)
        {
            writeln(line);
        }
    }
}
