struct S
{
@safe pure nothrow @nogc:

    this(float x)
    {
        x = x;                  // TODO warn, not error

        _x = _x;                // error
        this._x = _x;           // error
        _x = this._x;           // error

        _x = _y;

        _xp = _xp;              // warn
        _xp = _yp;
    }

    void foo()
    {
        _x = _x;                // error
        this._x = _x;           // error
        _x = this._x;           // error
    }

    this(this) { count += 1;}   // posblit

    int count;

    float _x;
    float _y;

    float* _xp;
    float* _yp;
}

pure nothrow @nogc unittest
{
    S s;
    s = s;

    S t;
    s._x = s._x;                // warn
    s._x = t._x;

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

int x;                          // global?

/**
 * See_Also: https://forum.dlang.org/post/cjccfvhbtbgnajplrvbd@forum.dlang.org
 */
void test() @safe nothrow @nogc
{
    int x = x;          // x is in another scope so this doesn't cause shadowing
}
