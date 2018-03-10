import std.meta : AliasSeq, NoDuplicates;
import traits_ex : allSame, allSameIterative, allSameTypeIterative, allSameTypeRecursive, allSameTypeHybrid;

struct W(T, size_t n)
{
    T value;
}

enum allSameUsingNoDuplicates(Ts...) = NoDuplicates!Ts.length == 1;

void main()
{
    alias differentTs = AliasSeq!(byte, ubyte, const(ubyte),
                                  short, ushort, const(ushort),
                                  int, uint, const(uint),
                                  long, ulong, const(ulong),
                                  float, double, real, const(real),
                                  cfloat, cdouble, creal, const(creal),
                                  char, wchar, dchar, const(dchar),
                                  string, wstring, dstring, const(dstring));
    alias sameTs = AliasSeq!(byte, byte,
                             byte, byte,
                             byte, byte,
                             byte, byte,
                             byte, byte, byte,
                             byte, byte, byte,
                             byte, byte, byte,
                             byte, byte,
                             byte);
    alias Ts = differentTs;

    pragma(msg, "Instantiation count: ", cast(int)Ts.length^^3);
    import std.stdio;

    foreach (T1; Ts)
    {
        foreach (T2; Ts)
        {
            foreach (T3; Ts)
            {
                // alias MergedTs = AliasSeq!(W!(T1, 1),
                //                            W!(T2, 2),
                //                            W!(T3, 3));
                // static if (is(MergedTs[0 .. $/2] ==
                //               MergedTs[$/2 .. $]))
                // {
                // }
                static if (allSameTypeIterative!(W!(T1, 1),
                                                 W!(T2, 2),
                                                 W!(T3, 3)))
                {
                }
            }
        }
    }
}
