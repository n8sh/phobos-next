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

/** Radix sort of $(D input).

    Note that this implementation of non-inplace radix sort only requires $(D
    input) to be a BidirectionalRange not a RandomAccessRange.

    Note that $(D input) can be a $(D BidirectionalRange) aswell as $(D
    RandomAccessRange).

    `radixBitCount` is the number of bits in radix (digit)

    TODO make `radixBitCount` a template parameter either 8 or 16,
    ElementType.sizeof must be a multiple of radixBitCount

    TODO input[] = y[] not needed when input is mutable

    TODO Restrict fun.

    TODO Choose fastDigitDiscardal based on elementMin and elementMax (if they
    are given)

    See also: https://probablydance.com/2016/12/27/i-wrote-a-faster-sorting-algorithm/
    See also: https://github.com/skarupke/ska_sort/blob/master/ska_sort.hpp
    See also: http://forum.dlang.org/thread/vmytpazcusauxypkwdbn@forum.dlang.org#post-vmytpazcusauxypkwdbn:40forum.dlang.org
 */
auto radixSort(R,
               alias fun = "a",
               bool descending = false,
               bool requestDigitDiscardal = false,
               bool inPlace = false)(R input,
                                     /* ElementType!R elementMin = ElementType!(R).max, */
                                     /* ElementType!R elementMax = ElementType!(R).min */)

    @trusted
    if (isRandomAccessRange!R &&
        (isNumeric!(ElementType!R)))
{
    import std.algorithm.sorting : isSorted, assumeSorted; // TODO move this to radixSort when know how map less to descending
    import std.algorithm : min, max;
    import std.range : front;

    immutable n = input.length; // number of elements
    alias E = ElementType!R;
    enum elementBitCount = 8*E.sizeof; // total number of bits needed to code each element

    /* Lookup number of radix bits from sizeof `ElementType`.
       These give optimal performance on Intel Core i7.
    */
    static if (elementBitCount == 8 ||
               elementBitCount == 24)
    {
        enum radixBitCount = 8;
    }
    else static if (elementBitCount == 16 ||
                    elementBitCount == 32 ||
                    elementBitCount == 64)
    {
        enum radixBitCount = 16;
    }
    else
    {
        static assert(0, "TODO handle element type " ~ e.stringof);
    }

    // TODO activate this: subtract min from all values and then immutable uint elementBitCount = is_min(a_max) ? 8*sizeof(E) : binlog(a_max); and add it back.
    enum digitCount = elementBitCount / radixBitCount;         // number of `digitCount` in radix `radixBitCount`
    static assert(elementBitCount % radixBitCount == 0,
                  "Precision of ElementType must be evenly divisble by bit-precision of Radix.");

    enum doDigitDiscardal = requestDigitDiscardal && digitCount >= 2;

    enum radix = cast(typeof(radixBitCount))1 << radixBitCount;    // bin count
    enum mask = radix-1;                                     // radix bit mask

    alias UE = typeof(input.front.bijectToUnsigned); // get unsigned integer type of same precision as \tparam E.

    import fixed_dynamic_array : FixedDynamicArray;

    static if (inPlace) // most-significant digit (MSD) first in-place radix sort
    {
        // histogram buckets count and later upper-limits/walls for values in `input`
        size_t[radix] binStat;
        foreach (immutable digitOffsetReversed; 0 .. digitCount) // for each `digitOffset` (in base `radix`) starting with least significant (LSD-first)
        {
            immutable digitOffset = digitCount - 1 - digitOffsetReversed;
            immutable digitBitshift = digitOffset*radixBitCount; // digit bit shift

            // calculate counts
            foreach (immutable j; 0 .. n) // for each element index `j` in `input`
            {
                immutable UE currentUnsignedValue = cast(UE)input[j].bijectToUnsigned(descending);
                immutable i = (currentUnsignedValue >> digitBitshift) & mask; // digit (index)
                ++binStat[i];   // increase histogram bin counter
            }
        }

        // bin boundaries: accumulate bin counters array
        foreach (immutable j; 1 .. radix) // for each successive bin counter
        {
            binStat[j] += binStat[j - 1]; // accumulate bin counter
        }

        assert(input.isSorted!"a < b");
    }
    else                        // standard radix sort
    {
        // non-in-place requires temporary `y`. TODO we could allocate these as
        // a stack-allocated array for small arrays and gain extra speed.
        auto tempStorage = FixedDynamicArray!E.makeUninitializedOfLength(n);
        auto tempSlice = tempStorage[];

        static if (doDigitDiscardal)
        {
            UE ors  = 0;         // digits diff(xor)-or-sum
        }

        foreach (immutable digitOffset; 0 .. digitCount) // for each `digitOffset` (in base `radix`) starting with least significant (LSD-first)
        {
            immutable digitBitshift = digitOffset*radixBitCount;   // digit bit shift

            static if (doDigitDiscardal)
            {
                if (digitOffset != 0) // if first iteration already performed we have bit statistics
                {
                    if ((! ((ors >> digitBitshift) & mask))) // if bits in digit[d] are either all \em zero or
                    {
                        continue;               // no sorting is needed for this digit
                    }
                }
            }

            // calculate counts
            size_t[radix] binStat; // histogram buckets count and later upper-limits/walls for values in `input`
            UE previousUnsignedValue = cast(UE)input[0].bijectToUnsigned(descending);
            foreach (immutable j; 0 .. n) // for each element index `j` in `input`
            {
                immutable UE currentUnsignedValue = cast(UE)input[j].bijectToUnsigned(descending);
                static if (doDigitDiscardal)
                {
                    if (digitOffset == 0) // first iteration calculates statistics
                    {
                        ors |= previousUnsignedValue ^ currentUnsignedValue; // accumulate bit change statistics
                        // ors |= currentUnsignedValue; // accumulate bits statistics
                    }
                }
                immutable i = (currentUnsignedValue >> digitBitshift) & mask; // digit (index)
                ++binStat[i];              // increase histogram bin counter
                previousUnsignedValue = currentUnsignedValue;
            }

            static if (doDigitDiscardal)
            {
                if (digitOffset == 0) // if first iteration already performed we have bit statistics
                {
                    if ((! ((ors >> digitBitshift) & mask))) // if bits in digit[d] are either all \em zero or
                    {
                        continue;               // no sorting is needed for this digit
                    }
                }
            }

            // bin boundaries: accumulate bin counters array
            foreach (immutable j; 1 .. radix) // for each successive bin counter
            {
                binStat[j] += binStat[j - 1]; // accumulate bin counter
            }

            // reorder. access `input`'s elements in \em reverse to \em reuse filled caches from previous forward iteration.
            // \em stable reorder from `input` to `tempSlice` using normal counting sort (see `counting_sort` above).
            enum unrollFactor = 1;
            assert((n % unrollFactor) == 0); // is evenly divisible by unroll factor
            for (size_t j = n - 1; j < n; j -= unrollFactor) // for each element `j` in reverse order. when `j` wraps around `j` < `n` is no longer true
            {
                version(LDC) static if (__VERSION__ >= 2076) { static assert(0, "TODO use static foreach inplace of iota!(...)"); }
                import range_ex : iota;
                foreach (k; iota!(0, unrollFactor)) // inlined (unrolled) loop
                {
                    immutable i = (input[j - k].bijectToUnsigned(descending) >> digitBitshift) & mask; // digit (index)
                    tempSlice[--binStat[i]] = input[j - k]; // reorder into tempSlice
                }
            }

            static if (digitCount & 1) // if odd number of digit passes
            {
                static if (__traits(compiles, input[] == tempSlice[]))
                {
                    input[] = tempSlice[]; // faster than std.algorithm.copy() because input never overlap tempSlice
                }
                else
                {
                    import std.algorithm : copy;
                    copy(tempSlice[], input[]); // TODO use memcpy
                }
            }
            else
            {
                import std.algorithm : swap;
                swap(input, tempSlice);
            }
        }
    }

    static if (descending)
    {
        return input.assumeSorted!"a > b";
    }
    else
    {
        return input.assumeSorted!"a < b";
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
        writef("%8-s, %10-s, ", E.stringof, n);

        import basic_uncopyable_array : Array = UncopyableArray;

        import std.traits : isIntegral, isSigned, isUnsigned;
        import random_ex : randInPlace, randInPlaceWithElementRange;
        import std.algorithm.sorting : sort, SwapStrategy, isSorted;
        import std.algorithm.comparison : min, max, equal;
        import std.range : retro;
        import std.datetime : StopWatch, AutoStart, TickDuration;
        auto sw = StopWatch();
        immutable nMax = 5;

        // generate random
        auto a = Array!E.withLength(n);
        static if (isUnsigned!E)
        {
            // a[].randInPlaceWithElementRange(cast(E)0, cast(E)uint.max);
            a[].randInPlace();
        }
        else
        {
            a[].randInPlace();
        }
        version(show) write("original random: ", a[0 .. min(nMax, $)], ", ");

        // standard quick sort
        auto qa = a.dup;

        sw.reset;
        sw.start();
        qa[].sort!("a < b", SwapStrategy.stable)();
        sw.stop;
        immutable TickDuration sortTime = sw.peek;
        version(show) write("quick sorted: ", qa[0 .. min(nMax, $)], ", ");
        assert(qa[].isSorted);

        // reverse radix sort
        {
            auto b = a.dup;
            b[].radixSort!(typeof(b[]), "a", true)();
            version(show) write("reverse radix sorted: ", b[0 .. min(nMax, $)], ", ");
            assert(b[].retro.equal(qa[]));
        }

        // standard radix sort
        {
            auto b = a.dup;

            sw.reset;
            sw.start();
            b[].radixSort!(typeof(b[]), "b", false)();
            sw.stop;
            immutable radixTime1 = sw.peek.usecs;

            writef("%9-s, ", cast(real)sortTime.usecs / radixTime1);
            assert(b[].equal(qa[]));
        }

        // standard radix sort fast-discardal
        {
            auto b = a.dup;

            sw.reset;
            sw.start();
            b[].radixSort!(typeof(b[]), "b", false, true)();
            sw.stop;
            immutable radixTime = sw.peek.usecs;

            assert(b[].equal(qa[]));

            version(show)
            {
                writeln("standard radix sorted with fast-discardal: ",
                        b[0 .. min(nMax, $)]);
            }
            writef("%9-s, ", cast(real)sortTime.usecs / radixTime);
        }

        // inplace-place radix sort
        // static if (is(E == uint))
        // {
        //     auto b = a.dup;

        //     sw.reset;
        //     sw.start();
        //     b[].radixSort!(typeof(b[]), "b", false, false, true)();
        //     sw.stop;
        //     immutable radixTime = sw.peek.usecs;

        //     assert(b[].equal(qa[]));

        //     version(show)
        //     {
        //         writeln("in-place radix sorted with fast-discardal: ",
        //                 b[0 .. min(nMax, $)]);
        //     }
        //     writef("%9-s, ", cast(real)sortTime.usecs / radixTime);
        // }

        writeln("");
    }

    import std.meta : AliasSeq;
    immutable n = 1_00_000;
    writeln("EType, eCount, radixSort (speed-up), radixSort with fast discardal (speed-up), in-place radixSort (speed-up)");
    foreach (immutable ix, T; AliasSeq!(byte, short, int, long))
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

    immutable n = 10_000;

    foreach (ix, E; AliasSeq!(byte, ubyte,
                              short, ushort,
                              int, uint,
                              long, ulong,
                              float, double))
    {
        import basic_uncopyable_array : Array = UncopyableArray;
        import std.algorithm.sorting : sort, isSorted;
        import std.algorithm.mutation : swap;
        import random_ex : randInPlace;

        auto a = Array!E.withLength(n);

        a[].randInPlace();
        auto b = a.dup;
        assert(a[].radixSort() == b[].sort());

        swap(a, b);
    }
}

version(unittest)
{
    import dbgio : dln;
}
