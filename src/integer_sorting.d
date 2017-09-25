#!/usr/bin/env rdmd-dev

/** Integer Sorting Algorithms.
    Copyright: Per Nordlöw 2017-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
 */
module integer_sorting;

import std.range : isRandomAccessRange, ElementType;
import std.traits : isNumeric;
import std.meta : AliasSeq;

import bijections;

/** Non-Inplace Radix Sort $(D x).

   Note that this implementation of non-inplace radix sort only requires $(D x)
   to be a BidirectionalRange not a RandomAccessRange.

   Note that $(D x) can be a $(D BidirectionalRange) aswell as $(D RandomAccessRange).

   radixNBits = Number of bits in Radix (Digit)

   TODO optimize calculcation of fast-digit discardal

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

    @trusted
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

    // TODO activate this: subtract min from all values and then const uint elemBits = is_min(a_max) ? 8*sizeof(E) : binlog(a_max); and add it back.
    enum nDigits = elemBits / radixNBits;         // number of \c nDigits in radix \p radixNBits
    static assert(elemBits % radixNBits == 0,
                  "Precision of ElementType must be evenly divisble by bit-precision of Radix.");

    /* const nRemBits = elemBits % radixNBits; // number remaining bits to sort */
    /* if (nRemBits) { nDigits++; }     // one more for remainding bits */

    enum radix = cast(typeof(radixNBits))1 << radixNBits;    // bin count
    enum mask = radix-1;                              // radix bit mask

    alias U = typeof(x.front.bijectToUnsigned); // get unsigned integer type of same precision as \tparam E.

    if (nDigits != 1)           // if more than one bucket sort pass (BSP)
    {
        doInPlace = false; // we cannot do in-place because each BSP is unstable and may ruin order from previous digit passes
    }

    static if (false/* doInPlace */)
    {
        // histogram buckets upper-limits/walls for values in \p x.
        Slice!size_t[radix] bins = void; // bucket slices
        for (uint d = 0; d != nDigits; ++d)  // for each digit-index \c d (in base \c radix) starting with least significant (LSD-first)
        {
            const uint sh = d*radixNBits;   // digit bit shift

            // TODO activate and verify that performance is unchanged.
            // auto uize_ = [descending, sh, mask](E a) { return (bijectToUnsigned(a, descending) >> sh) & mask; }; // local shorthand

            // reset histogram counters
            bins[] = 0;

            // populate histogram \c hist for current digit
            U ors  = 0;             // digits "or-sum"
            U ands = ~ors;          // digits "and-product"

            for (size_t j = 0; j != n; ++j) // for each element index \c j in \p x
            {
                const uint i = (x[j].bijectToUnsigned(descending) >> sh) & mask; // digit (index)
                ++bins[i].high();       // increase histogram bin counter
                ors |= i;               // accumulate all one bits statistics
                ands &= i;              // accumulate all zero bits statistics
            }

            if ((! ors) ||      // if bits in digit[d] are all zero or
                (! ~ands))      // if bits in digit[d] are all one
            {
                continue;               // no sorting is needed for this digit
            }

            // bin boundaries: accumulate bin counters array
            size_t bin_max = bins[0].high();
            bins[0].low() = 0;                    // first floor is always zero
            for (size_t j = 1; j != radix; ++j)  // for each successive bin counter
            {
                bin_max = max(bin_max, bins[j].high());
                bins[j].low()  = bins[j - 1].high(); // previous roof becomes current floor
                bins[j].high() += bins[j - 1].high(); // accumulate bin counter
            }
            // TODO if (bin_max == 1) { writeln("No accumulation needed!"); }

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
                        const int rN = (e0.bijectToUnsigned(descending) >> sh) & mask; // next digit (index)
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
        // histogram buckets upper-limits/walls for values in \p x.
        size_t[radix] hist; // most certainly fits in the stack (L1-cache)

        // non-in-place requires temporary \p y. TODO we could allocate these as
        // a stack-allocated array for small arrays and gain extra speed.
        import fixed_dynamic_array : FixedDynamicArray;
        auto tempStorage = FixedDynamicArray!E.makeUninitialized(n);
        auto y = tempStorage[];

        foreach (const d; 0 .. nDigits) // for each digit-index \c d (in base \c radix) starting with least significant (LSD-first)
        {
            const sh = d*radixNBits;   // digit bit shift

            // reset histogram counters
            hist[] = 0;

            // populate histogram \c hist for current digit
            static if (fastDigitDiscardal)
            {
                U ors  = 0;             // digits "or-sum"
                U ands = ~(cast(U)0);   // digits "and-product"
            }
            for (size_t j = 0; j != n; ++j) // for each element index \c j in \p x
            {
                const uint i = (x[j].bijectToUnsigned(descending) >> sh) & mask; // digit (index)
                ++hist[i];              // increase histogram bin counter
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

            // bin boundaries: accumulate bin counters array
            for (size_t j = 1; j != radix; ++j) // for each successive bin counter
            {
                hist[j] += hist[j - 1]; // accumulate bin counter
            }

            // reorder. access \p x's elements in \em reverse to \em reuse filled caches from previous forward iteration.
            // \em stable reorder from \p x to \c y using normal counting sort (see \c counting_sort above).
            enum unrollFactor = 1;
            assert((n % unrollFactor) == 0); // is evenly divisible by unroll factor
            for (size_t j = n - 1; j < n; j -= unrollFactor) // for each element \c j in reverse order. when j wraps around j < n is no longer true
            {
                version(LDC) static if (__VERSION__ >= 2076) { static assert(0, "TODO use static foreach inplace of iota!(...)"); }
                import range_ex : iota;
                foreach (k; iota!(0, unrollFactor))
                {
                    const uint i = (x[j - k].bijectToUnsigned(descending) >> sh) & mask; // digit (index)
                    y[--hist[i]] = x[j - k]; // reorder into y
                }
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
                    copy(y[], x[]); // TODO use memcpy
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

version = benchmark;
// version = show;

version(benchmark)
@safe unittest
{
    import std.stdio : write, writef, writeln;

    /** Test $(D radixSort) with ElementType $(D E) */
    void test(E)(int n) @safe
    {
        writef("E:%8-s n:%10-s: ", E.stringof, n);

        import random_ex : randInPlace;
        import std.algorithm : sort, min, max, isSorted;
        import std.range : retro;
        import std.algorithm : equal;
        import std.datetime : StopWatch, AutoStart, TickDuration;
        auto sw = StopWatch();
        immutable nMax = 5;

        // generate random
        auto a = new E[n];
        a[].randInPlace();
        version(show) write("original random: ", a[0 .. min(nMax, $)], ", ");

        // standard quick sort
        TickDuration sortTime;
        auto qa = a.dup;
        sw.reset; sw.start(); sort(qa); sw.stop; sortTime = sw.peek;
        version(show) write("quick sorted: ", qa[0 .. min(nMax, $)], ", ");
        assert(qa.isSorted);

        // reverse radix sort
        {
            auto b = a.dup;
            radixSort!(typeof(b), "a", true)(b);
            version(show) write("reverse radix sorted: ", b[0 .. min(nMax, $)], ", ");
            assert(b.retro.equal(qa));
        }

        // standard radix sort
        {
            auto b = a.dup;

            sw.reset;
            sw.start();
            radixSort!(typeof(b), "b", false)(b);
            sw.stop;
            immutable radixTime1 = sw.peek.usecs;

            writef("radixSort: %9-s, ", cast(real)sortTime.usecs / radixTime1);
            assert(b.equal(qa));
        }

        // standard radix sort fast-discardal
        {
            auto b = a.dup;

            sw.reset;
            sw.start();
            radixSort!(typeof(b), "b", false, true)(b);
            sw.stop;
            immutable radixTime = sw.peek.usecs;

            assert(b.equal(qa));

            version(show)
            {
                writeln("standard radix sorted with fast-discardal: ",
                        b[0 .. min(nMax, $)]);
            }
            writef("radixSort with fast-discardal: %9-s ", cast(real)sortTime.usecs / radixTime);
        }

        writeln("");
    }

    import std.meta : AliasSeq;
    const n = 1_00_000;
    foreach (ix, T; AliasSeq!(byte, short, int, long))
    {
        test!T(n); // test signed
        import std.traits : Unsigned;
        test!(Unsigned!T)(n); // test unsigned
    }
    test!float(n);
    test!double(n);
}

@safe unittest
{
    import std.meta : AliasSeq;

    const n = 1_000_000;

    foreach (ix, T; AliasSeq!(byte, short))
    {
        import basic_uncopyable_array : Array = UncopyableArray;
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

version(unittest)
{
    import dbgio : dln;
}
