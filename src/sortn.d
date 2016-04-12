#!/usr/bin/env rdmd-dev-module

/** Fixed Length Sorting via Sorting Networks.

    See also: http://forum.dlang.org/post/ne5m62$1gu5$1@digitalmars.com
    See also: http://cpansearch.perl.org/src/JGAMBLE/Algorithm-Networksort-1.30/lib/Algorithm/Networksort.pm

    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).

    TODO Stability of equal elements: Need template parameter `equalityStability`? Scalar builtin values are always stable.
*/
module sortn;

import std.meta : allSatisfy;
import std.traits : isIntegral;
import std.range : isInputRange, isRandomAccessRange;

/** Static Iota. */
template iota(size_t from, size_t to)
    if (from <= to)
{
    alias iota = iotaImpl!(to-1, from);
}
private template iotaImpl(size_t to, size_t now)
{
    import std.meta : AliasSeq;
    static if (now >= to) { alias iotaImpl = AliasSeq!(now); }
    else                  { alias iotaImpl = AliasSeq!(now, iotaImpl!(to, now+1)); }
}

/** Conditionally pairwise sort elements of `Range` `r` at indexes `Indexes`
    using comparison `less`.
 */
void conditionalSwap(alias less = "a < b", Range, Indexes...)(Range r)
    if (isRandomAccessRange!Range &&
        allSatisfy!(isIntegral, typeof(Indexes)) &&
        Indexes.length &&
        (Indexes.length & 1) == 0) // even number of indexes
{
    import std.algorithm.mutation : swapAt;
    import std.functional : binaryFun;
    alias comp = binaryFun!less; //< comparison
    enum n = Indexes.length / 2; // number of replacements
    foreach (const i; iota!(0, n))
    {
        const j = Indexes[2*i];
        const k = Indexes[2*i + 1];
        if (!comp(r[j], r[k]))
        {
            r.swapAt(j, k);
        }
    }
}

/** Largest length supported by network sort `sortUpTo`. */
enum sortUpToMaxLength = 22;

/** Sort at most then first `n` elements of `r` using comparison `less`.

    Note: Sorting networks for `n` >= 3 are not unique.

    See also: http://stackoverflow.com/questions/3903086/standard-sorting-networks-for-small-values-of-n
    See also: http://www.cs.brandeis.edu/~hugues/sorting_networks.html
 */
auto sortUpTo(uint n, alias less = "a < b", Range)(Range r) // TODO uint or size_t?
    if (isRandomAccessRange!Range)
in
{
    assert(r.length <= n);
}
body
{
    auto s = r[0 .. n];

    static if (n < 2)
    {
        // already sorted
    }
    else static if (n == 2)
    {
        s.conditionalSwap!(less, Range, 0, 1);
    }
    else static if (n == 3)
    {
        s.conditionalSwap!(less, Range,
                           0,1,
                           1,2,
                           0,1);
    }
    else static if (n == 4)
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, // 2 in parallel
                           0,2, 1,3, // 2 in parallel
                           1,2);
    }
    else static if (n == 5)
    {
        s.conditionalSwap!(less, Range,
                           0,1, 3,4,  // 2 in parallel
                           0,2,
                           1,2, 0,3,  // 2 in parallel
                           2,3, 1,4,  // 2 in parallel
                           1,2, 3,4); // 2 in parallel
    }
    else static if (n == 6)
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, // 3 in parallel
                           0,2, 1,4, 3,5, // 3 in parallel
                           0,1, 2,3, 4,5, // 3 in parallel
                           1,2, 3,4,      // 2 in parallel
                           2,3,
            );
    }
    else static if (n == 9)     // R. W. Floyd.
    {
        s.conditionalSwap!(less, Range,
                           0,1, 3,4, 6,7, 1,2, 4,5, 7,8, 0,1, 3,4, 6,7,
                           0,3, 3,6, 0,3, 1,4, 4,7, 1,4, 2,5, 5,8, 2,5,
                           1,3, 5,7, 2,6, 4,6, 2,4, 2,3, 5,6);
    }
    else static if (n == 10)    // A. Waksman.
    {
        s.conditionalSwap!(less, Range,
                           4,9, 3,8, 2,7, 1,6, 0,5, 1,4, 6,9, 0,3, 5,8,
                           0,2, 3,6, 7,9, 0,1, 2,4, 5,7, 8,9,
                           1,2, 4,6, 7,8, 3,5, 2,5, 6,8, 1,3, 4,7,
                           2,3, 6,7, 3,4, 5,6, 4,5);
    }
    else static if (n == 11)    // 12-input by Shapiro and Green, minus the connections to a twelfth input.
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9,
                           1,3, 5,7, 0,2, 4,6, 8,10,
                           1,2, 5,6, 9,10, 1,5, 6,10,
                           5,9, 2,6, 1,5, 6,10, 0,4,
                           3,7, 4,8, 0,4, 1,4, 7,10, 3,8,
                           2,3, 8,9, 2,4, 7,9, 3,5, 6,8, 3,4, 5,6, 7,8);
    }
    else static if (n == 12)    // Shapiro and Green.
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9, 10,11,
                           1,3, 5,7, 9,11, 0,2, 4,6, 8,10,
                           1,2, 5,6, 9,10, 1,5, 6,10,
                           5,9, 2,6, 1,5, 6,10, 0,4, 7,11,
                           3,7, 4,8, 0,4, 7,11, 1,4, 7,10, 3,8,
                           2,3, 8,9, 2,4, 7,9, 3,5, 6,8, 3,4, 5,6, 7,8);
    }
    else static if (n == 13)    // Generated by the END algorithm.
    {
        s.conditionalSwap!(less, Range,
                           1,7, 9,11, 3,4, 5,8, 0,12, 2,6,
                           0,1, 2,3, 4,6, 8,11, 7,12, 5,9,
                           0,2, 3,7, 10,11, 1,4, 6,12, 7,8,
                           11,12, 4,9, 6,10,
                           3,4, 5,6, 8,9, 10,11, 1,7, 2,6,
                           9,11, 1,3, 4,7, 8,10,
                           0,5, 2,5, 6,8, 9,10, 1,2, 3,5, 7,8,
                           4,6,
                           2,3, 4,5, 6,7, 8,9, 3,4, 5,6);
    }
    else static if (n == 14) // Green's construction for 16 inputs minus connections to the fifteenth and sixteenth inputs.
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13,
                           0,2, 4,6, 8,10, 1,3, 5,7, 9,11,
                           0,4, 8,12, 1,5, 9,13, 2,6, 3,7,
                           0,8, 1,9, 2,10, 3,11, 4,12, 5,13,
                           5,10, 6,9, 3,12,
                           7,11, 1,2, 4,8, 1,4, 7,13, 2,8,
                           2,4, 5,6, 9,10, 11,13, 3,8, 7,12,
                           6,8, 10,12, 3,5, 7,9,
                           3,4, 5,6, 7,8, 9,10, 11,12, 6,7, 8,9);
    }
    else static if (n == 15) // Green's construction for 16 inputs minus connections to the sixteenth input.
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13,
                           0,2, 4,6, 8,10, 12,14, 1,3, 5,7, 9,11,
                           0,4, 8,12, 1,5, 9,13, 2,6, 10,14, 3,7,
                           0,8, 1,9, 2,10, 3,11, 4,12, 5,13, 6,14,
                           5,10, 6,9, 3,12, 13,14,
                           7,11, 1,2, 4,8, 1,4, 7,13, 2,8, 11,14,
                           2,4, 5,6, 9,10, 11,13, 3,8, 7,12,
                           6,8, 10,12, 3,5, 7,9,
                           3,4, 5,6, 7,8, 9,10, 11,12, 6,7, 8,9);
    }
    else static if (n == 16) // Green's construction.
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13, 14,15,
                           0,2, 4,6, 8,10, 12,14, 1,3, 5,7, 9,11, 13,15,
                           0,4, 8,12, 1,5, 9,13, 2,6, 10,14, 3,7, 11,15,
                           0,8, 1,9, 2,10, 3,11, 4,12, 5,13, 6,14, 7,15,
                           5,10, 6,9, 3,12, 13,14,
                           7,11, 1,2, 4,8, 1,4, 7,13, 2,8, 11,14,
                           2,4, 5,6, 9,10, 11,13, 3,8, 7,12,
                           6,8, 10,12, 3,5, 7,9,
                           3,4, 5,6, 7,8, 9,10, 11,12, 6,7, 8,9);
    }
    else static if (n == 18) // Baddar's PHD thesis, chapter 6. Fewest stages but 2 comparators more than 'batcher'
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13, 14,15, 16,17,
                           0,2, 1,3, 4,6, 5,7, 8,10, 9,11, 12,17, 13,14, 15,16,
                           0,4, 1,5, 2,6, 3,7, 9,10, 8,12, 11,16, 13,15, 14,17,
                           7,16, 6,17, 3,5, 10,14, 11,12, 9,15, 2,4, 1,13, 0,8,
                           16,17, 7,14, 5,12, 3,15, 6,13, 4,10, 2,11, 8,9, 0,1,
                           1,8, 14,16, 6,9, 7,13, 5,11, 3,10, 4,15, 4,8, 14,15,
                           5,9, 7,11, 1,2, 12,16, 3,6, 10,13, 5,8, 11,14, 2,3,
                           12,13, 6,7, 9,10, 7,9, 3,5, 12,14, 2,4, 13,15, 6,8,
                           10,11, 13,14, 11,12, 9,10, 7,8, 5,6, 3,4, 12,13,
                           10,11, 8,9, 6,7, 4,5);
    }
    else static if (n == 22) // Baddar's PHD thesis, chapter 7. Fewest stages but 2 comparators more than 'batcher'
    {
        s.conditionalSwap!(less, Range,
                           0,1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13, 14,15, 16,17,
                           18,19, 20,21, 2,4, 1,3, 0,5, 6,8, 7,9, 10,12, 11,13,
                           14,16, 15,17, 18,20, 19,21, 6,10, 7,11, 8,12, 9,13,
                           14,18, 15,19, 16,20, 17,21, 3,5, 1,4, 0,2, 9,17,
                           7,15, 11,19, 8,16, 3,12, 0,10, 1,18, 5,20, 13,21,
                           6,14, 2,4, 0,7, 17,20, 3,15, 9,18, 2,11, 4,16, 5,10,
                           1,8, 12,19, 13,14, 20,21, 0,6, 3,8, 12,18, 2,13,
                           14,16, 5,9, 10,15, 4,7, 11,17, 16,20, 18,19, 15,17,
                           12,14, 10,11, 7,9, 8,13, 4,5, 1,3, 2,6, 19,20,
                           16,17, 15,18, 11,14, 9,13, 10,12, 7,8, 3,5, 4,6,
                           1,2, 18,19, 14,16, 13,15, 11,12, 8,9, 5,10, 6,7,
                           2,3, 17,19, 16,18, 14,15, 12,13, 9,11, 8,10, 5,7,
                           3,6, 2,4, 17,18, 15,16, 13,14, 11,12, 9,10, 7,8,
                           5,6, 3,4, 16,17, 14,15, 12,13, 10,11, 8,9, 6,7,
                           4,5);
    }
    else
    {
        static assert(false, "Unsupported n " ~ n.stringof);
    }

    import std.algorithm.sorting : assumeSorted;
    return s.assumeSorted!less;
}

/** Hybrid sort `r` using sortUpTo for if length of `r` is less than or equal to
    `sortUpToMaxLength` and `std.algorithm.sorting.sort` otherwise.
 */
auto hybridSort(alias less = "a < b", Range)(Range r) // TODO uint or size_t?
    if (isRandomAccessRange!Range)
{
    import std.algorithm.sorting : isSorted;
    foreach (uint n; iota!(2, sortUpToMaxLength + 1))
    {
        static if (__traits(compiles, { r.sortUpTo!(n, less); }))
        {
            // pragma(msg, n);
            if (n == r.length)
            {
                auto s = r.sortUpTo!(n, less);
                debug assert(s.isSorted!less);
                return s;
            }
        }
    }
    import std.algorithm.sorting : sort;
    return sort!less(r);
}

///
@safe pure unittest
{
    import std.algorithm.sorting : isSorted;
    import std.algorithm.iteration : permutations;
    import std.range : iota;
    import std.random : randomShuffle, Random;

    Random random;

    import dbg : dln;

    alias T = uint;

    const maxPermutationLength = 9;
    const maxPermutations = 10_000;

    import std.meta : AliasSeq;
    foreach (less; AliasSeq!("a < b", "a > b"))
    {
        foreach (const n; iota(0, sortUpToMaxLength + 1))
        {
            if (n >= maxPermutationLength) // if number of elements is too large
            {
                foreach (x; iota(0, maxPermutations))
                {
                    import std.array : array;
                    auto y = iota(0, n).array;
                    y.randomShuffle(random);
                    y.hybridSort!less;
                    assert(y.isSorted!less);
                }
            }
            else
            {
                foreach (x; iota(0, n).permutations)
                {
                    import std.array : array;
                    auto y = x.array;
                    y.hybridSort!less;
                    assert(y.isSorted!less);
                }
            }
        }
    }
}
