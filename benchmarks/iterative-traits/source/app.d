import std.meta : AliasSeq, NoDuplicates;
import traits_ex : allSame, allSameTypeIterative, allSameTypeRecursive;

struct W(T, size_t n)
{
    T value;
}

enum allSameUsingNoDuplicates(Ts...) = NoDuplicates!Ts.length == 1;

void main()
{
    alias differentTs = AliasSeq!(byte, ubyte,
                                  short, ushort,
                                  int, uint,
                                  long, ulong,
                                  float, double, real,
                                  cfloat, cdouble, creal,
                                  char, wchar, dchar,
                                  string, wstring, dstring);
    alias sameTs = AliasSeq!(byte, byte,
                             byte, byte,
                             byte, byte,
                             byte, byte,
                             byte, byte, byte,
                             byte, byte, byte,
                             byte, byte, byte,
                             byte, byte);
    alias Ts = differentTs;

    pragma(msg, "Instantiation count: ", cast(int)Ts.length^^3);
    import std.stdio;

    foreach (T1; Ts)
    {
        foreach (T2; Ts)
        {
            foreach (T3; Ts)
            {
                static if (allSameUsingNoDuplicates!(W!(T1, 1),
                                                     W!(T2, 2),
                                                     W!(T3, 3)))
                {
                }
                else
                {
                }
            }
        }
    }
}
