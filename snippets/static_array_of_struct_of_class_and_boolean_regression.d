class C
{
    this(int i) { this.i = i; }
    int i;
}

struct S
{
    this(C c)
    {
        this.c = c;
    }

    C c;
    version(DigitalMars)
    {
        /** DMD has bug in the codegen
         *
         * See_also: https://forum.dlang.org/post/modqrqwsqvhxodtlickm@forum.dlang.org
         */
        ulong _dummy;
    }
    bool b;                  // removing this prevents bug
}

pragma(msg, S.sizeof);

void main()
{
    import std.stdio : writeln;
    import std.array : staticArray;

    auto c = new C(42);

    const S[1] s1 = [S(c)].staticArray;
    const S[1] s2 = [S(c)];

    writeln(cast(void*)s1[0].c);
    writeln(cast(void*)s2[0].c);

    assert(s1[0].c is
           s2[0].c);
}
