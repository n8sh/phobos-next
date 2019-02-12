class C
{
    this(int i) { this.i = i; }
    int i;
}

struct S
{
    C c;
    bool b;                  // removing this prevents bug
}

unittest
{
    import std.stdio : writeln;
    import std.array : staticArray;

    auto c = new C(42);

    auto s1 = [S(c)].staticArray;
    S[1] s2 = [S(c)];

    writeln(cast(void*)s1[0].c);
    writeln(cast(void*)s2[0].c);

    assert(s1[0].c is
           s2[0].c);
}
