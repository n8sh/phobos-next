struct S
{
@safe pure nothrow @nogc:

    struct T
    {
        int _z;
    }

    this(float x, T t)
    {
        x = x;                  // warn
        t = t;                  // warn

        _x = _x;                // error
        this._x = _x;           // error
        _x = this._x;           // error

        _x = _y;

        _xp = _xp;              // error
        _xp = _yp;

        _t = _t;                // error
        _t._z = _t._z;          // error (transitive)
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

    T _t;
}

pure nothrow @nogc void test1()
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

    (&*xp) = (&*xp);            // warn

    (*&x) = (*&x);              // warn

    (*&*&x) = (*&*&x);          // warn

    static assert(__traits(compiles, { int t; t = t; }));
}

int g_x;

alias g_y = g_x;

/**
 * See_Also: https://forum.dlang.org/post/cjccfvhbtbgnajplrvbd@forum.dlang.org
 */
@safe nothrow @nogc void test2()
{
    int x;
    x = g_x;          // x is in another scope so this doesn't cause shadowing
    g_x = g_x;        // warn
    g_y = g_y;        // warn
}

struct U
{
@safe pure nothrow @nogc:
    void opAssign(U) {}
}

@safe pure nothrow @nogc void test2()
{
    U u;
    u = u;
}

@system unittest
{
    import std.exception : assertThrown;
    import std.typecons : Nullable;
    Nullable!int a;
    assert(a.isNull);
    assertThrown!Throwable(a.get);
    a = 5;
    assert(!a.isNull);
    assert(a == 5);
    assert(a != 3);
    assert(a.get != 3);
    a.nullify();
    assert(a.isNull);
    a = 3;
    assert(a == 3);
    a *= 6;
    assert(a == 18);
    auto b = a;
    a = b;
    assert(a == 18);
    a.nullify();
    assertThrown!Throwable(a += 2);
}
