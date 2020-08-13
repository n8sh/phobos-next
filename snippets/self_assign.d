struct S
{
@safe pure nothrow @nogc:
    this(this) { count += 1;}   // posblit
    int count;
}

pure nothrow unittest
{
    S s;
    s = s;

    int x;
    x = x;                      // diagnose

    int y;
    y = x;

    int* xp;
    xp = xp;                    // diagnose

    *xp = *xp;                  // diagnose

    *&x = *&x;                  // diagnose

    *&*&x = *&*&x;              // diagnose

    static assert(__traits(compiles, { int t; t = t; }));
}

int x;
void test() @safe nothrow @nogc
{
    int x = x;                  // shouldn't this give a shadowing warning?
}
