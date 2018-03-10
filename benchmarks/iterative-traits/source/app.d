struct W(T, size_t n)
{
    T value;
}

void main()
{
    import std.meta : AliasSeq, NoDuplicates;
    import traits_ex : allSame, allSameTypeIterative, allSameTypeRecursive;

    alias Ts = AliasSeq!(byte, ubyte,
                         short, ushort,
                         int, uint,
                         long, ulong,
                         float, double, real,
                         cfloat, cdouble, creal,
                         char, wchar, dchar,
                         string, wstring, dstring);

    pragma(msg, "Instantiation count: ", cast(int)Ts.length^^4);
    import std.stdio;

    auto count = 0;
    foreach (T1; Ts)
    {
        foreach (T2; Ts)
        {
            foreach (T3; Ts)
            {
                foreach (T4; Ts)
                {
                    count += allSameTypeIterative!(W!(T1, 1),
                                                   W!(T2, 2),
                                                   W!(T3, 3),
                                                   W!(T4, 4)) ? 1 : 0;
                }
            }
        }
    }
}
