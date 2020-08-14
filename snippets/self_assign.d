struct S
{
@safe pure nothrow @nogc:
    this(float x)
    {
        x = x;                  // warning

        _x = _x;                // error
        this._x = _x;           // error
        _x = this._x;           // error

        _x = _y;
    }
    this(this) { count += 1;}   // posblit
    int count;
    float _x;
    float _y;
}

pure nothrow unittest
{
    S s;
    s = s;

    int x;
    x = x;                      // warn

    int y;
    y = x;

    int* xp;
    xp = xp;                    // warn

    *xp = *xp;                  // warn

    (*&x) = (*&x);              // warn

    (*&*&x) = (*&*&x);          // warn

    static assert(__traits(compiles, { int t; t = t; }));
}

int x;
void test() @safe nothrow @nogc
{
    int x = x;                  // shouldn't this give a shadowing warning?
}
