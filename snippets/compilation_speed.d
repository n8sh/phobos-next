bool startsWith(T)(scope const(T)[] haystack,
                   scope const(T)[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[0 .. needle.length] == needle; // range check is elided by LDC in release builds
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(char, wchar, dchar,
                          short, ushort,
                          int, uint,
                          long, ulong,
                          float, double, real))
    {
        T[] x;
        T[] y;
        assert(x.startsWith(y));
    }
}
