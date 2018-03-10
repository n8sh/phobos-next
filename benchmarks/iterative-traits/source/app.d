struct W(T, size_t n)
{
    T value;
}

void main()
{
    import std.meta : AliasSeq, NoDuplicates;
    import traits_ex : allSame, allSameTypeRecursive;

    alias Ts = AliasSeq!(byte, ubyte,
                         short, ushort,
                         int, uint,
                         long, ulong,
                         float, double, real,
                         char, wchar, dchar,
                         string, wstring, dstring);

    pragma(msg, "Instantiation count: ", cast(int)Ts.length^^3);
    import std.stdio;

    auto count = 0;
    static foreach (T1; Ts)
    {
        static foreach (T2; Ts)
        {
            static foreach (T3; Ts)
            {
                count += allSame!(W!(T1, 1),
                                  W!(T2, 2),
                                  W!(T3, 3)) ? 1 : 0;
                // writeln(T1.stringof, " - ", T2.stringof, " - ", T3.stringof, " => ", count);
            }
        }
    }
}
