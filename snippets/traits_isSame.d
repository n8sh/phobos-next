@safe unittest
{
    static assert( __traits(isSame, int, int));
    static assert(!__traits(isSame, int, short));

    enum a = 1, b = 1, c = 2, s = "a", t = "a";
    static assert( __traits(isSame, 1, 1));
    static assert( __traits(isSame, a, 1));
    static assert( __traits(isSame, a, b));
    static assert(!__traits(isSame, b, c));
    static assert( __traits(isSame, "a", "a"));
    static assert( __traits(isSame, s, "a"));
    static assert( __traits(isSame, s, t));
    static assert(!__traits(isSame, 1, "1"));
    static assert(!__traits(isSame, a, "a"));
    static assert( __traits(isSame, isSame, isSame));
    static assert(!__traits(isSame, isSame, a));

    static assert(!__traits(isSame, byte, a));
    static assert(!__traits(isSame, short, isSame));
    static assert(!__traits(isSame, a, int));
    static assert(!__traits(isSame, long, isSame));

    static immutable X = 1, Y = 1, Z = 2;
    static assert( __traits(isSame, X, X));
    static assert(!__traits(isSame, X, Y));
    static assert(!__traits(isSame, Y, Z));

    int  foo();
    int  bar();
    real baz(int);
    static assert( __traits(isSame, foo, foo));
    static assert(!__traits(isSame, foo, bar));
    static assert(!__traits(isSame, bar, baz));
    static assert( __traits(isSame, baz, baz));
    static assert(!__traits(isSame, foo, 0));

    int  x, y;
    real z;
    static assert( __traits(isSame, x, x));
    static assert(!__traits(isSame, x, y));
    static assert(!__traits(isSame, y, z));
    static assert( __traits(isSame, z, z));
    static assert(!__traits(isSame, x, 0));
}
