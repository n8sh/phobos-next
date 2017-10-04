module modulo;

import std.traits : isIntegral;

/** Lookup type representing an unsigned integer in inclusive range (0 .. m - 1).
    TODO Merge with similar logic in bound.d
 */
template TypeOfModulo(size_t m)
{
    static      if (m - 1 <= ubyte.max)  { alias TypeOfModulo = ubyte; }
    else static if (m - 1 <= ushort.max) { alias TypeOfModulo = ushort; }
    else static if (m - 1 <= uint.max)   { alias TypeOfModulo = uint; }
    else                                 { alias TypeOfModulo = ulong; }
    // TODO ucent?
}

/** Module type within inclusive value range (0 .. `m`-1).

    Similar to Ada's modulo type `0 mod m`.

    See also: https://forum.dlang.org/post/hmrpwyqfoxwtywbznbrr@forum.dlang.org
    See also: http://codeforces.com/contest/628/submission/16212299

    TODO reuse ideas from bound.d

    TODO Add function limit()
    static if (isPowerOf2!m)
    {
    return x & 2^^m - 1;
    }
    else
    {
    return x % m;
    }

    invariant
    {
        assert (0 <= x && x < m);
    }

    called after opBinary opUnary etc similar to what is done
    http://codeforces.com/contest/628/submission/16212299

    TODO Move to Phobos std.typecons
 */
template Mod(size_t m,
             T = TypeOfModulo!m)
    if (m >= 1 &&
        isIntegral!T)
{
    import math_ex : isPowerOf2;

    // smallest builtin unsigned integer type that can fit 2^^m
    alias UI = TypeOfModulo!m;

    static assert(m - 1 <= 2^^(8*T.sizeof) - 1); // if so, check that it matches `s`

    // `this` uses exactly as many bits as storage type `T`
    enum exactStorageTypeMatch = m == 2^^(T.sizeof);

    struct Mod
    {
        enum min = 0;
        enum max = m - 1;

        /// Construct from `value` of unsigned integer type `UI`.
        this(U)(U value)
            if (isIntegral!U)
        in
        {
            static if (m != 2^^(U.sizeof)) // dynamic check only modulo doesn't equal storage precision
            {
                assert(value < m, `value too large`); // TODO use enforce instead?
            }
        }
        body
        {
            this.x = cast(T)value; // overflow checked in ctor
        }

        /// Construct from Mod!n, where `m >= n`.
        this(size_t n, U)(Mod!(n, U) rhs)
            if (m >= n && isIntegral!U)
        {
            this.x = cast(T)rhs.x; // cannot overflow
        }

        /// Assign from `value` of unsigned integer type `UI`.
        auto ref opAssign(U)(U value)
            if (isIntegral!U)
        in
        {
            static if (m != 2^^(U.sizeof)) // dynamic check only modulo doesn't equal storage precision
            {
                assert(value < m, `value too large`); // TODO use enforce instead?
            }
        }
        body
        {
            this.x = cast(T)value; // overflow checked in ctor
        }

        /// Assign from Mod!n, where `m >= n`.
        auto ref opAssign(size_t n, U)(Mod!(n, U) rhs)
            if (m >= n && isIntegral!U)
        {
            this.x = cast(T)rhs.x; // cannot overflow
        }

        auto ref opOpAssign(string op, U)(U rhs)
            if (op == `+` ||
                op == `-` ||
                op == `*` &&
                isIntegral!U)
        {
            mixin(`x = cast(T)(x ` ~ op ~ `rhs);`);
            return this;
        }

        auto opUnary(string op, string file = __FILE__, int line = __LINE__)()
        {
            static      if (op == `+`)
            {
                return this;
            }
            else static if (op == `--`)
            {
                static if (isPowerOf2(m))
                {
                    x = (x - 1) & max; // more efficient
                }
                else
                {
                    if (x == min) x = max; else --x;
                }
                return this;
            }
            else static if (op == `++`)
            {
                static if (isPowerOf2(m))
                {
                    x = (x + 1) & max; // more efficient
                }
                else
                {
                    if (x == max) x = min; else ++x;
                }
                return this;
            }
            else
            {
                static assert(`Unsupported unary operator `, op);
            }
        }

        @property size_t _prop() const @safe pure nothrow @nogc { return x; } // read-only access
        alias _prop this;

        private T x;
    }
}

/// Instantiator for `Mod`.
auto mod(size_t m, T = TypeOfModulo!m)(T value)
    if (m >= 1)
{
    return Mod!(m)(value);
}

///
@safe pure nothrow @nogc unittest
{
    enum m = 256;
    Mod!(m, ubyte) ub = m - 1;
    Mod!(m, uint) ui = m - 1;
    assert(ub == ui);
    --ub;
    assert(ub != ui);
    ui = ub;
    assert(ub == ui);
    --ui;
    assert(ub != ui);
    ub = ui;
    assert(ub == ui);
}

///
@safe pure nothrow @nogc unittest
{
    static assert(is(typeof(Mod!3(1)) ==
                     typeof(1.mod!3)));

    // check size logic
    static assert(Mod!(ubyte.max + 1).sizeof == 1);
    static assert(Mod!(ubyte.max + 2).sizeof == 2);
    static assert(Mod!(ushort.max + 1).sizeof == 2);
    static assert(Mod!(ushort.max + 2).sizeof == 4);
    static assert(Mod!(cast(size_t)uint.max + 1).sizeof == 4);
    static assert(Mod!(cast(size_t)uint.max + 2).sizeof == 8);

    // assert that storage defaults to packed unsigned integer
    static assert(is(Mod!(8, ubyte) == Mod!(8)));

    Mod!(8, ubyte) x = 6;
    static assert(x.min == 0);
    static assert(x.max == 7);
    Mod!(8, ubyte) y = 7;
    static assert(y.min == 0);
    static assert(y.max == 7);
    // static assert(!__traits(compiles, { Mod!(8) z = 256; }));
    // static assert(!__traits(compiles, { Mod!(8, ubyte) z = 256; }));

    assert(x < y);

    y = 5;
    y = 5L;

    assert(y < x);

    assert(y == 5);
    assert(y != 0);

    y++;
    assert(y == 6);
    assert(++y == y.max);
    assert(y++ == y.max);
    assert(y == y.min);
    assert(--y == y.max);
    assert(++y == y.min);

    Mod!(8, uint) ui8 = 7;
    Mod!(256, ubyte) ub256 = 255;
    ub256 = ui8;    // can assign to larger modulo from higher storage precision

    Mod!(258, ushort) ub258 = ub256;

    // copy construction to smaller modulo is disallowed
    static assert(!__traits(compiles, { Mod!(255, ubyte) ub255 = ub258; }));

    auto a = 7.mod!10;
    auto b = 8.mod!256;
    auto c = 257.mod!1000;

    static assert(c.min == 0);
    static assert(c.max == 999);

    assert(a < b);
    assert(a < c);

    // assignment to larger modulo (super-type/set) is allowed
    b = a;
    c = a;
    c = b;

    // assignment to smaller modulo (sub-type/set) is disallowed
    static assert(!__traits(compiles, { a = b; }));
    static assert(!__traits(compiles, { a = c; }));
}

///
@safe pure nothrow @nogc unittest
{
    Mod!(256, ubyte) x = 55;

    x += 200;
    assert(x == 255);

    x += 1;
    assert(x == 0);

    x -= 4;
    assert(x == 252);

    const Mod!(256, ubyte) y = 55;

    import std.traits : isAssignable;
    static assert(isAssignable!(typeof(x), typeof(y)));
    x = y;
}

/// construct from other precision
@safe pure nothrow @nogc unittest
{
    enum m = 256;
    Mod!(m, ubyte) x = 55;
    Mod!(m, uint) y = x;
    Mod!(m, ubyte) z = y;
}
