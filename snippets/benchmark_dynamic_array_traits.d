/// See_Also: https://github.com/dlang/dmd/pull/9014#issuecomment-674451700
/// See_Also: https://github.com/dlang/phobos/pull/7574

import std.meta : AliasSeq;

// version = useBuiltin;           ///< Use new builtin trait __traits(isDynamicArray, ...)

private static alias ScalarTypes = AliasSeq!(bool,
                                             char, wchar, dchar,
                                             byte, ubyte,
                                             short, ushort,
                                             int, uint,
                                             long, ulong,
                                             float, double, real,
                                             cfloat, cdouble, creal,
                                             ifloat, idouble, ireal,
                                             string, wstring, dstring);

private static enum qualifiers = AliasSeq!("", "const", "inout", "immutable", "shared", "shared const");

static private template isDynamicArray(T)
{
    static if (is(T == U[], U))
        enum bool isDynamicArray = true;
    else static if (is(T U == enum))
        enum bool isDynamicArray = isDynamicArray!U;
    else
        enum bool isDynamicArray = false;
}

@safe pure unittest
{
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
                mixin("enum ",
                      T, "_", U, "_", V, // type
                      " ",
                      "e_", "_", T, "_", U, "_", V, // name
                      " = ",
                      T, "_", U, "_", V, ".init",
                      ";");
                version(useBuiltin)
                    static assert(__traits(isDynamicArray, mixin(T, "_", U, "_", V)[])); // min over 10 runs: 2.62s.
                else
                {
                    // static assert(is(mixin(qualifier, "(", T, "_", U, "_", V, ")")[] == X[], X)); // min over 10 runs: 2.75s
                    static assert(isDynamicArray!(mixin(T, "_", U, "_", V)[])); // min over 10 runs: 3.19s
                }
            }
        }
    }

}
version(none)                   // this is slower than above
template ctBenchmark(Types = ScalarTypes)
{
    void ctBenchmark()
    {
        static foreach (T; Types)
        {
            static foreach (U; Types)
            {
                static foreach (V; Types)
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
                        {
                            alias OuterType = mixin(qualifier, "(", T, "_", U, "_", V, ")");
                            version(useBuiltin)
                                static assert(__traits(isDynamicArray, OuterType[])); // min over 10 runs: 1.38 s
                            else
                                static assert(is(OuterType[] == X[], X)); // min over 10 runs: 1.42 s
                        }
                    }
                }

            }
        }
    }
}

version(none)
@safe pure unittest
{
    ctBenchmark!()();
}
