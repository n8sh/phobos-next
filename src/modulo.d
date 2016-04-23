module modulo;                  // haha ;)

/** Module type within inclusive value range `(0 .. m-1)`.

    Similar to Ada's modulo type `0 mod m`.

    TODO Move to std.typecons
    TODO reuse ideas from bound.d (moved here)
 */
struct Mod(size_t m, T = uint)
{
    static assert(m <= 2^^(8*T.sizeof));

    this(T value)
    in
    {
        assert(value < m); // TODO use enforce instead?
    }
    body
    {
        this.x = value;
    }

    auto ref opAssign(T value)
    in
    {
        assert(value < m); // TODO use enforce instead?
    }
    body
    {
        this.x = value;
    }

    @property size_t prop(){ return x; }

    alias prop this;
    private T x;
}

///
unittest
{
    Mod!(8, ubyte) x = 6;
    Mod!(8, ubyte) y = 7;

    assert(x < y);

    y = 5;

    assert(y < x);

    assert(y == 5);
    assert(y != 0);

    Mod!(8, uint) ui8 = 7;
    Mod!(256, ubyte) ub256 = 255;
}
