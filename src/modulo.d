module modulo;                  // haha ;)

import std.traits : isIntegral;

/** Module type within inclusive value range `(0 .. m-1)`.

    Similar to Ada's modulo type `0 mod m`.

    See also: https://forum.dlang.org/post/hmrpwyqfoxwtywbznbrr@forum.dlang.org
    See also: http://codeforces.com/contest/628/submission/16212299

    TODO Add modulo logic
    TODO Move to Phobos std.typecons
    TODO reuse ideas from bound.d (moved here)

    TODO Add function limit()
    static if (isPow2!m)
    {
    return x & 2^^m - 1;
    }
    else
    {
    return x % m;
    }

    called after opBinary opUnary etc.

 */
template Mod(size_t m, T = void)
    if (is(T == void) || isIntegral!T)
{
    static assert(m > 0, "m must be greater than zero");

    static if (!is(T == void))
    {
        static assert(m - 1 <= 2^^(8*T.sizeof) - 1);
        alias S = T;
    }
    else static if (m - 1 <= ubyte.max)  { alias S = ubyte; }
    else static if (m - 1 <= ushort.max) { alias S = ushort; }
    else static if (m - 1 <= uint.max)   { alias S = uint; }
    else                                 { alias S = ulong; }

    struct Mod
    {
        this(U)(U value)
            if (isIntegral!U)
        in
        {
            assert(value < m, "value too large"); // TODO use enforce instead?
        }
        body
        {
            this.x = cast(S)value; // TODO ok to cast here?
        }

        auto ref opAssign(U)(U value)
            if (isIntegral!U)
        in
        {
            assert(value < m, "value too large"); // TODO use enforce instead?
        }
        body
        {
            this.x = cast(S)value; // TODO ok to cast here?
        }

        @property size_t prop() const
        {
            return x;
        }

        alias prop this;
        private S x;
    }
}

/// Instantiator for `Mod`.
auto mod(size_t m, T)(T value)
    if (is(T == void) || isIntegral!T)
{
    return Mod!(m, T)(value);
}

///
@safe pure nothrow @nogc unittest
{
    // check size logic
    static assert(Mod!(ubyte.max + 1).sizeof == 1);
    static assert(Mod!(ubyte.max + 2).sizeof == 2);
    static assert(Mod!(ushort.max + 1).sizeof == 2);
    static assert(Mod!(ushort.max + 2).sizeof == 4);
    static assert(Mod!(cast(size_t)uint.max + 1).sizeof == 4);
    static assert(Mod!(cast(size_t)uint.max + 2).sizeof == 8);

    Mod!(8, ubyte) x = 6;
    Mod!(8, ubyte) y = 7;

    assert(x < y);

    y = 5;
    y = 5L;

    assert(y < x);

    assert(y == 5);
    assert(y != 0);

    Mod!(8, uint) ui8 = 7;
    Mod!(256, ubyte) ub256 = 255;

    const a = 7.mod!10;
    const b = 8.mod!256;
    const c = 257.mod!1000;

    assert(a < b);
    assert(a < c);
}
