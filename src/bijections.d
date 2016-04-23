module bijections;

import std.meta : AliasSeq;

import std.traits : isUnsigned, isSigned, isIntegral, isFloatingPoint, Unsigned, Signed, isNumeric;

alias IntegralBijectableTypes = AliasSeq!(ubyte, ushort, uint, ulong,
                                          byte, short, int, long,
                                          char, wchar, dchar,
                                          float, double);

enum isRadixSortableElementType(T) = false; // TODO Among(T, IntegralBijectableTypes);

/** Biject (Shift) Signed $(D a) "up" to Unsigned (before radix sorting). */
@trusted pure nothrow auto bijectToUnsigned(T)(T a)
if (isNumeric!T)
{
    static if (isIntegral!T)
    {
        static      if (isSigned!T)
            return a + (cast(Unsigned!T)1 << (8*T.sizeof - 1)); // "add up""
        else static if (isUnsigned!T)
            return a;           // identity
        else
            static assert(false, "Unsupported integral input type " ~ T.stringof);
    }
    else static if (isFloatingPoint!T)
    {
        static      if (is(T == float))
            return ff(*cast(uint*)(&a));
        else static if (is(T == double))
            return ff(*cast(ulong*)(&a));
        else
            static assert(false, "Unsupported floating point input type " ~ T.stringof);
    }
    else
        static assert(false, "Unsupported input type " ~ T.stringof);
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
