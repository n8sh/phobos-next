/// See_Also: https://github.com/dlang/dmd/pull/9014#issuecomment-674451700
/// See_Also: https://github.com/dlang/phobos/pull/7574

import std.meta : AliasSeq;

private static alias ScalarTypes = AliasSeq!(bool,
                                             char, wchar, dchar,
                                             byte, ubyte,
                                             short, ushort,
                                             int, uint,
                                             long, ulong,
                                             float, double, real,
                                             cfloat, cdouble, creal,
                                             ifloat, idouble, ireal);

private static enum qualifiers = AliasSeq!("", "const", "inout", "immutable");

static foreach (T; ScalarTypes)
{
    static foreach (U; ScalarTypes)
    {
        static foreach (V; ScalarTypes)
        {
            mixin("struct ",
                  T, "_" ,U, "_" ,V,
                  " {",
                  T, " t; ",
                  U, " u; ",
                  V, " v; ",
                  "}");
            static foreach (qualifier; qualifiers)
            {
                static assert(__traits(isDynamicArray, mixin(qualifier, "(", T, "_", U, "_", V, ")")[]));
            }
        }

    }
}
