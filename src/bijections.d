/** TOOD extract reinterpret!T(x)
    TODO real
 */
module bijections;

static if (__VERSION__ >= 2071)
{
    import std.meta : Unqual;
}
else
{
    import std.typecons : Unqual;
}

import std.meta : AliasSeq, staticIndexOf;
import std.traits : isUnsigned, isSigned, isIntegral, Unsigned, Signed, isNumeric, isSomeChar;

/** List of types that are bijectable to builtin integral types. */
alias IntegralBijectableTypes = AliasSeq!(char, wchar, dchar,
                                          ubyte, ushort, uint, ulong,
                                          byte, short, int, long,
                                          float, double);

enum isIntegralBijectableType(T) = staticIndexOf!(Unqual!T, IntegralBijectableTypes) >= 0;

/** Biject (Shift) Signed $(D a) "up" to Unsigned (before radix sorting). */
auto bijectToUnsigned(T)(T a) @trusted pure nothrow
    if (isIntegralBijectableType!T)
{
    alias UT = Unqual!T;

    static      if (is(UT == char))  { return *(cast(ubyte*)&a); } // reinterpret
    else static if (is(UT == wchar)) { return *(cast(ushort*)&a); } // reinterpret
    else static if (is(UT == dchar)) { return *(cast(uint*)&a); } // reinterpret
    else static if (isIntegral!UT)
    {
        static      if (isSigned!UT)
        {
            alias UUT = Unsigned!UT;
            return cast(UUT)(a + (cast(UUT)1 << (8*UT.sizeof - 1))); // "add up""
        }
        else static if (isUnsigned!UT)
        {
            return a;           // identity
        }
        else
        {
            static assert(false, "Unsupported integral input type " ~ UT.stringof);
        }
    }
    else static if (is(UT == float))  { return ff(*cast(uint*)(&a)); }
    else static if (is(UT == double)) { return ff(*cast(ulong*)(&a)); }
    else static assert(false, "Unsupported input type " ~ UT.stringof);
}

@safe @nogc pure nothrow
{
    auto bijectToUnsigned(T)(T a, bool descending)
    {
        immutable ua = a.bijectToUnsigned;
        return descending ? ua.max-ua : ua;
    }

    /** Biject (Shift) Unsigned  $(D a) "back down" to Signed (after radix sorting). */
    void bijectFromUnsigned(U)(U a, ref Signed!U b)
        if (isUnsigned!T)
    {
        b = a - (cast(Unsigned!T)1 << (8*U.sizeof - 1)); // "add down""
    }
    void bijectFromUnsigned(U)(U a, ref U b)
        if (isUnsigned!U)
    {
        b = a;                  ///< Identity.
    }

    /** Map a Floating Point Number \p a Back from Radix Sorting
     * (Inverse of \c radix_flip_float()).
     * - if sign is 1 (negative), it flips the sign bit back
     * - if sign is 0 (positive), it flips all bits back
     */

    /** Map Bits of Floating Point Number \p a to Unsigned Integer that can be Radix Sorted.
     * Also finds \em sign of \p a.
     * - if it's 1 (negative float), it flips all bits.
     * - if it's 0 (positive float), it flips the sign only.
     */
    @safe pure nothrow uint    ff(uint f) { return f ^ (-cast(int)  (f >> (32-1))      | 0x80000000); }
    @safe pure nothrow uint   iff(uint f) { return f ^            (((f >> (32-1)) - 1) | 0x80000000); }
    @safe pure nothrow ulong  ff(ulong f) { return f ^ (-cast(long) (f >> (64-1))      | 0x8000000000000000); }
    @safe pure nothrow ulong iff(ulong f) { return f ^            (((f >> (64-1)) - 1) | 0x8000000000000000); }

    @trusted pure nothrow void bijectFromUnsigned(ubyte a, ref char b) { b = *cast(typeof(b)*)(&a); }
    @trusted pure nothrow void bijectFromUnsigned(ushort a, ref wchar b) { b = *cast(typeof(b)*)(&a); }
    @trusted pure nothrow void bijectFromUnsigned(ulong a, ref dchar b) { b = *cast(typeof(b)*)(&a); }

    @trusted pure nothrow void bijectFromUnsigned(uint a,  ref float  b) { uint  t = iff(a); b = *cast(float*)(&t); }
    @trusted pure nothrow void bijectFromUnsigned(ulong a, ref double b) { ulong t = iff(a); b = *cast(double*)(&t); }
}

@safe @nogc pure nothrow unittest
{
    foreach (T; AliasSeq!(ubyte, ushort, uint, ulong))
    {
        static assert(is(typeof(T.init.bijectToUnsigned) == T));
    }
    foreach (T; AliasSeq!(byte, short, int, long))
    {
        static assert(is(typeof(T.init.bijectToUnsigned) == Unsigned!T));
    }

    static assert(is(typeof(char.init.bijectToUnsigned) == ubyte));
    static assert(is(typeof(wchar.init.bijectToUnsigned) == ushort));
    static assert(is(typeof(dchar.init.bijectToUnsigned) == uint));

    const n = 1_000_000;
    import std.range : iota;
    foreach (const i; 0.iota(n))
    {
        foreach (T; AliasSeq!(char, wchar, dchar,
                              ubyte, ushort, uint, ulong,
                              float, double))
        {
            const T x = cast(T)i;
            const y = x.bijectToUnsigned;
            // pragma(msg, "T:", T);
            // pragma(msg, "typeof(x):", typeof(x));
            // pragma(msg, "typeof(y):", typeof(y));
            T z; y.bijectFromUnsigned(z);
            assert(x == z);
        }
    }
}
