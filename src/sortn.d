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
enum sortUpToMaxLength = 12;

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

@safe pure // nothrow
unittest
{
    import std.algorithm.sorting : isSorted;
    import std.algorithm.iteration : permutations;
    import std.range : iota;
    import std.random : randomShuffle, Random;

    Random random;

    import dbg : dln;

    alias T = uint;

    const maxPermutationLength = 9;
    const maxPermutations = 1_00_000;

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
