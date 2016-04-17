#!/usr/bin/env rdmd-dev

/** Integer Sorting Algorithms.
    Copyright: Per Nordlöw 2014-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
 */
module intsort;

import std.range : isBidirectionalRange, isRandomAccessRange, ElementType;
import std.traits : isUnsigned, isSigned, isIntegral, isFloatingPoint, Unsigned, Signed, isNumeric;
import std.meta : AliasSeq;

alias RadixSortableElementTypes = AliasSeq!(ubyte, ushort, uint, ulong,
                                            byte, short, int, long,
                                            char, wchar, dchar,
                                            float, double);

enum isRadixSortableElementType(T) = false; // TODO Among(T, RadixSortableElementTypes);

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

/**
   Non-Inplace Radix Sort $(D x).

   Note that this implementation of non-inplace radix sort only requires $(D x)
   to be a BidirectionalRange not a RandomAccessRange.

   Note that $(D x) can be a $(D BidirectionalRange) aswell as $(D RandomAccessRange).

   Params:
     radixNBits = Number of bits in Radix (Digit)

   TODO x[] = y[] not needed when input is mutable
   TODO Restrict fun.
   TODO Choose fastDigitDiscardal based on elementMin and elementMax (if they
   are given)
   TODO Add doInPlace CT-param. If doInPlace isRandomAccessRange!R is needed

   TODO Use @nogc attribute when possible

   See also: http://forum.dlang.org/thread/vmytpazcusauxypkwdbn@forum.dlang.org#post-vmytpazcusauxypkwdbn:40forum.dlang.org
 */
auto radixSort(R,
               alias fun = "a",
               bool descending = false,
               bool fastDigitDiscardal = false)(R x,
                                                bool doInPlace = false/* , */
                                                /* ElementType!R elementMin = ElementType!(R).max, */
                                                /* ElementType!R elementMax = ElementType!(R).min */)
    if (isRandomAccessRange!R &&
        (isNumeric!(ElementType!R)))
{
    import std.algorithm : min, max;
    import std.range : front;

    immutable n = x.length; // number of elements
    alias E = ElementType!R;
    enum elemBits = 8*E.sizeof; // Total Number of Bits needed to code each element

    /* Lookup number of radix bits from sizeof ElementType.
       These give optimal performance on Intel Core i7.
    */
    static if (elemBits == 8)
    {
        enum radixNBits = 8;
    }
    else static if (elemBits == 16 ||
                    elemBits == 32 ||
                    elemBits == 64)
    {
        enum radixNBits = 16; // this prevents "rest" digit
    }
    else
    {
        static assert("Cannot handle ElementType " ~ E.stringof);
    }

    // TODO Activate this: subtract min from all values and then const uint elemBits = is_min(a_max) ? 8*sizeof(E) : binlog(a_max); and add it back.
    enum nDigits = elemBits / radixNBits;         // Number of \c nDigits in radix \p radixNBits
    static assert(elemBits % radixNBits == 0,
                  "Precision of ElementType must be evenly divisble by bit-precision of Radix.");

    /* const nRemBits = elemBits % radixNBits; // number remaining bits to sort */
    /* if (nRemBits) { nDigits++; }     // one more for remainding bits */

    enum radix = cast(typeof(radixNBits))1 << radixNBits;    // Bin Count
    enum mask = radix-1;                              // radix bit mask

    alias U = typeof(bijectToUnsigned(x.front)); // Get Unsigned Integer Type of same precision as \tparam E.

    if (nDigits != 1)           // if more than one bucket sort pass (BSP)
    {
        doInPlace = false; // we cannot do in-place because each BSP is unstable and may ruin order from previous digit passes
    }

    static if (false/* doInPlace */)
    {
        // Histogram Buckets Upper-Limits/Walls for values in \p x.
        Slice!size_t[radix] bins = void; // bucket slices
        for (uint d = 0; d != nDigits; ++d)  // for each digit-index \c d (in base \c radix) starting with least significant (LSD-first)
        {
            const uint sh = d*radixNBits;   // digit bit shift

            // TODO Activate and verify that performance is unchanged.
            // auto uize_ = [descending, sh, mask](E a) { return (bijectToUnsigned(a, descending) >> sh) & mask; }; // local shorthand

            // Reset Histogram Counters
            bins[] = 0;

            // Populate Histogram \c O for current digit
            U ors  = 0;             // digits "or-sum"
            U ands = ~ors;          // digits "and-product"
            for (size_t j = 0; j != n; ++j) // for each element index \c j in \p x
            {
                const uint i = (bijectToUnsigned(x[j], descending) >> sh) & mask; // digit (index)
                ++bins[i].high();       // increase histogram bin counter
                ors |= i; ands &= i; // accumulate bits statistics
            }
            if ((! ors) || (! ~ands)) { // if bits in digit[d] are either all \em zero or all \em one
                continue;               // no sorting is needed for this digit
            }

            // Bin Boundaries: Accumulate Bin Counters Array
            size_t bin_max = bins[0].high();
            bins[0].low() = 0;                    // first floor is always zero
            for (size_t j = 1; j != radix; ++j)  // for each successive bin counter
            {
                bin_max = max(bin_max, bins[j].high());
                bins[j].low()  = bins[j - 1].high(); // previous roof becomes current floor
                bins[j].high() += bins[j - 1].high(); // accumulate bin counter
            }
            // TODO if (bin_max == 1) { std::cout << "No accumulation needed!" << std::endl; }

            /** \em Unstable In-Place (Permutate) Reorder/Sort \p x.
             * Access \p x's elements in \em reverse to \em reuse filled caches from previous forward iteration.
             * \see \c in_place_indexed_reorder
             */
            for (int r = radix - 1; r >= 0; --r) // for each radix digit r in reverse order (cache-friendly)
            {
                while (bins[r])  // as long as elements left in r:th bucket
                {
                    const uint i0 = bins[r].pop_back(); // index to first element of permutation
                    const E    e0 = x[i0]; // value of first/current element of permutation
                    while (true)
                    {
                        const int rN = (bijectToUnsigned(e0, descending) >> sh) & mask; // next digit (index)
                        if (r == rN) // if permutation cycle closed (back to same digit)
                            break;
                        const ai = bins[rN].pop_back(); // array index
                        swap(x[ai], e0); // do swap
                    }
                    x[i0] = e0;         // complete cycle
                }
            }
        }

    }
    else
    {
        // Histogram Buckets Upper-Limits/Walls for values in \p x.
        size_t[radix] O; // most certainly fits in the stack (L1-cache)

        // Non-In-Place requires temporary \p y. TODO We could allocate these as
        // a stack-allocated array for small arrays and gain extra speed.
        import std.array : uninitializedArray;
        auto y = uninitializedArray!(E[])(n);

        foreach (d; 0 .. nDigits) // for each digit-index \c d (in base \c radix) starting with least significant (LSD-first)
        {
            const sh = d*radixNBits;   // digit bit shift

            // Reset Histogram Counters
            O[] = 0;

            // Populate Histogram \c O for current digit
            static if (fastDigitDiscardal)
            {
                U ors  = 0;             // digits "or-sum"
                U ands = ~ors;          // digits "and-product"
            }
            for (size_t j = 0; j != n; ++j) // for each element index \c j in \p x
            {
                const uint i = (bijectToUnsigned(x[j], descending) >> sh) & mask; // digit (index)
                ++O[i];              // increase histogram bin counter
                static if (fastDigitDiscardal)
                {
                    // accumulate bits statistics
                    ors |= i;
                    ands &= i;
                }
            }
            static if (fastDigitDiscardal)
            {
                if ((! ors) || (! ~ands)) // if bits in digit[d] are either all \em zero or all \em one
                {
                    continue;               // no sorting is needed for this digit
                }
            }

            // Bin Boundaries: Accumulate Bin Counters Array
            for (size_t j = 1; j != radix; ++j) // for each successive bin counter
            {
                O[j] += O[j - 1]; // accumulate bin counter
            }

            // Reorder. Access \p x's elements in \em reverse to \em reuse filled caches from previous forward iteration.
            // \em Stable Reorder From \p x to \c y using Normal Counting Sort (see \c counting_sort above).
            for (size_t j = n - 1; j < n; --j) // for each element \c j in reverse order. when j wraps around j < n is no longer true
            {
                const uint i = (bijectToUnsigned(x[j], descending) >> sh) & mask; // digit (index)
                y[--O[i]] = x[j]; // reorder into y
            }

            static if (nDigits & 1) // if odd number of digit passes
            {
                static if (__traits(compiles, x[] == y[]))
                {
                    x[] = y[]; // faster than std.algorithm.copy() because x never overlap y
                }
                else
                {
                    import std.algorithm : copy;
                    copy(y[], x[]);
                }
            }
            else
            {
                import std.algorithm : swap;
                swap(x, y);
            }
        }
    }

    import std.algorithm.sorting : assumeSorted; // TODO move this to radixSort when know how map less to descending
    static if (descending)
    {
        return x.assumeSorted!"a > b";
    }
    else
    {
        return x.assumeSorted!"a < b";
    }
}

@safe unittest
{
    import std.stdio : writeln;

/** Test $(D radixSort) with ElementType $(D E) */
    void test(E)(int n) @safe
    {
        writeln("ElementType=", E.stringof, " n=", n);

        immutable show = true;
        import random_ex : randInPlace;
        import std.algorithm : sort, min, max, isSorted;
        import std.range : retro;
        import std.algorithm : equal;
        import std.datetime : StopWatch, AutoStart, TickDuration;
        auto sw = StopWatch();
        immutable nMax = 5;

        // Generate Random
        auto a = new E[n];
        a[].randInPlace;
        if (show) writeln("original random: ", a[0 .. min(nMax, $)]);

        // Quick Sort
        TickDuration sortTime;
        auto qa = a.dup;
        sw.reset; sw.start(); sort(qa); sw.stop; sortTime = sw.peek;
        if (show) writeln("quick sorted: ", qa[0 .. min(nMax, $)]);
        assert(qa.isSorted);

        // Reverse Radix Sort
        {
            auto b = a.dup;
            radixSort!(typeof(b), "a", true)(b);
            if (show) writeln("reverse radix sorted: ", b[0 .. min(nMax, $)]);
            assert(b.retro.equal(qa));
        }

        // Standard Radix Sort
        {
            auto b = a.dup;
            sw.reset; sw.start(); radixSort!(typeof(b), "b", false)(b); sw.stop;
            immutable radixTime1 = sw.peek.usecs;
            if (show)
            {
                writeln("standard radix sorted: ",
                        b[0 .. min(nMax, $)]);
                writeln("- sort:", sortTime.usecs,
                        "us radixSort:", radixTime1,
                        "us Speed-Up:", cast(real)sortTime.usecs / radixTime1);
            }
            assert(b.equal(qa));
        }

        // Standard Radix Sort Fast-Discardal
        {
            auto b = a.dup;
            sw.reset; sw.start(); radixSort!(typeof(b), "b", false, true)(b); sw.stop;
            assert(b.equal(qa));
            immutable radixTime = sw.peek.usecs;
            if (show)
            {
                writeln("standard radix sorted with fast-discardal: ",
                        b[0 .. min(nMax, $)]);
                writeln("- sort:", sortTime.usecs,
                        "us radixSort:", radixTime,
                        "us Speed-Up:", cast(real)sortTime.usecs / radixTime);
            }
        }

        writeln("");
    }

    import std.meta : AliasSeq;
    const n = 1_000_000;
    foreach (ix, T; AliasSeq!(byte, short, int, long))
    {
        test!T(n); // test signed
        test!(Unsigned!T)(n); // test unsigned
    }
    test!float(n);
    test!double(n);
}

unittest
{
    import std.meta : AliasSeq;

    const n = 1_000_000;

    foreach (ix, T; AliasSeq!(byte, short))
    {
        import std.container : Array;
        import std.algorithm : sort, isSorted, swap;
        import random_ex : randInPlace;

        auto a = Array!T();
        a.length = n;

        a[].randInPlace;

        auto b = a.dup;

        radixSort(a[]);
        assert(a[].isSorted);

        b[].sort;
        assert(b[].isSorted);

        assert(a == b);

        swap(a, b);
    }
}
