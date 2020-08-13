struct S
{
pure nothrow:
    this(this) { count += 1;}   // posblit
    int count;
}
pure unittest
{
    S s;
    assert(s.count == 0);
    s = s;
    assert(s.count == 1);

    int x;
    x = x;                      // diagnose

    int y;
    y = x;

    int* xp;
    xp = xp;                    // diagnose

    *xp = *xp;                  // diagnose

    *(&x) = *(&x);              // should diagnose

    static assert(__traits(compiles, { int t; t = t; }));
}

int x;
void test() @safe nothrow @nogc
{
    int x = x;                  // shouldn't this give a shadowing warning?
}
