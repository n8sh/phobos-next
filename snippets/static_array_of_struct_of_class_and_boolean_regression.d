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

void main()
{
    import std.stdio : writeln;
    import std.array : staticArray;

    auto c = new C(42);

    const S[1] s1 = [S(c)];
    const S[1] s2 = [S(c)].staticArray;

    writeln(cast(void*)s1[0].c);
    writeln(cast(void*)s2[0].c);

    assert(s1[0].c is
           s2[0].c);
}
