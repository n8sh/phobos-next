/// See_Also: https://github.com/dlang/dmd/pull/9014#issuecomment-674451700
/// See_Also: https://github.com/dlang/phobos/pull/7574

import std.meta : AliasSeq;

static foreach (T; AliasSeq!(bool,
                             char, wchar, dchar,
                             byte, ubyte,
                             short, ushort,
                             int, uint,
                             long, ulong,
                             float, double, real,
                             cfloat, cdouble, creal,
                             ifloat, idouble, ireal))
{
    static foreach (U; AliasSeq!(bool,
                                 char, wchar, dchar,
                                 byte, ubyte,
                                 short, ushort,
                                 int, uint,
                                 long, ulong,
                                 float, double, real,
                                 cfloat, cdouble, creal,
                                 ifloat, idouble, ireal))
    {
        mixin("struct ",
              T, "_" ,U,
              " {",
              T, " t; ",
              U, " u; ",
              "}");
        static assert(__traits(isDynamicArray, mixin(T,"_",U)[]));
        // static assert(__traits(isDynamicArray, const(T)[]));
        // static assert(__traits(isDynamicArray, inout(T)[]));
        // static assert(__traits(isDynamicArray, immutable(T)[]));
    }
}
