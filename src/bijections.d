module bijections;

import std.meta : AliasSeq, staticIndexOf;

import std.traits : isUnsigned, isSigned, isIntegral, Unsigned, Signed, isNumeric;

/** List of types that are bijectable to builtin integral types. */
alias IntegralBijectableTypes = AliasSeq!(char, wchar, dchar,
                                          ubyte, ushort, uint, ulong, // TODO ucent?
                                          byte, short, int, long, // TODO cent?
                                          float, double); // TODO real?

enum isIntegralBijectableType(T) = staticIndexOf!(T, IntegralBijectableTypes) >= 0;

/** Biject (Shift) Signed $(D a) "up" to Unsigned (before radix sorting). */
auto bijectToUnsigned(T)(T a) @trusted pure nothrow
    if (isIntegralBijectableType!T)
{
    import std.meta : Unqual;
    alias U = Unqual!T;

    static      if (is(U == char))  return *(cast(ubyte*)&a); // reinterpret
    else static if (is(U == wchar)) return *(cast(ushort*)&a); // reinterpret
    else static if (is(U == dchar)) return *(cast(uint*)&a); // reinterpret
    else static if (isIntegral!U)
    {
        static      if (isSigned!U)
            return a + (cast(Unsigned!U)1 << (8*U.sizeof - 1)); // "add up""
        else static if (isUnsigned!U)
            return a;           // identity
        else
            static assert(false, "Unsupported integral input type " ~ U.stringof);
    }
    else static if (is(U == float))  return ff(*cast(uint*)(&a));
    else static if (is(U == double)) return ff(*cast(ulong*)(&a));
    else static assert(false, "Unsupported input type " ~ U.stringof);
}

@safe @nogc pure nothrow
{
    auto bijectToUnsigned(T)(T a, bool descending)
    {
        immutable ua = bijectToUnsigned(a);
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
    @trusted pure nothrow void bijectFromUnsigned(uint a,  ref float  b) { uint  t = iff(a); b = *cast(float*)(&t); }
    @trusted pure nothrow void bijectFromUnsigned(ulong a, ref double b) { ulong t = iff(a); b = *cast(double*)(&t); }
}
