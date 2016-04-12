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

/** Conditional pairwise swap elements of `Range` `r` at indexes `Indexes` using
    comparison `less`.
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
enum maxNetworkSortLength = 6;

/** Sort at most then first `n` elements of `r` using comparison `less`.
   See also: http://stackoverflow.com/questions/3903086/standard-sorting-networks-for-small-values-of-n
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
    else
    {
        static assert(n > maxNetworkSortLength); // check that maxNetworkSortLength is
        static assert(false, "Range must contain at most " ~ maxNetworkSortLength.stringof ~ " elements");
    }

    import std.algorithm.sorting : assumeSorted;
    return s.assumeSorted;
}

/** Hybrid sort `r`.
 */
auto hybridSort(alias less = "a < b", Range)(Range r) // TODO uint or size_t?
    if (isRandomAccessRange!Range)
{
    import std.algorithm.sorting : isSorted;
    foreach (uint n; iota!(2, maxNetworkSortLength + 1))
    {
        if (n == r.length)
        {
            auto s = r.sortUpTo!(n, less);
            debug assert(s.isSorted);
            return s;
        }
    }
    import std.algorithm.sorting : sort;
    return sort(r);
}

@safe pure nothrow unittest
{
    import dbg : dln;
    import std.algorithm.sorting : isSorted;

    alias T = uint;

    import std.range : iota;
    foreach (uint n; iota(0, maxNetworkSortLength + 1))
    {
        auto xs = new int[n];
        import std.range : iota;

        foreach (const i; 0.iota(n))
        {
            xs[i] = cast(T)(n - i);
        }

        dln("before:", xs[]);
        xs[].hybridSort;
        dln("after:", xs[]);
        assert(xs[].isSorted);
    }

}

/** Assign-Sort-4 `a`, `b`, `c` and `d` into `k`, `l`, `m` and `n` .
 * Time-Complexity: (\em Small!): 5 CMP, 4 MOV
 * Code Complexity: (\em Large!): 23 CMP, 24*4 MOV
 * NOTE Uses value-semantic on input arguments to enable move-semantics.
 */
void asort4(T)(T a, T b, T c, T d,
               ref T k, ref T l, ref T m, ref T n)
{
    if (c < d)
        if (b < d)
            if (b < c)
                if (a < c)
                    if (a < b)
                    { k=a; l=b; m=c; n=d; }
                    else
                    { k=b; l=a; m=c; n=d; }
                else
                    if (a < d)
                    { k=b; l=c; m=a; n=d; }
                    else
                    { k=b; l=c; m=d; n=a; }
            else
                if (a < b)
                    if (a < c)
                    { k=a; l=c; m=b; n=d; }
                    else
                    { k=c; l=a; m=b; n=d; }
                else
                    if (a < d)
                    { k=c; l=b; m=a; n=d; }
                    else
                    { k=c; l=b; m=d; n=a; }
        else
            if (a < d)
                if (a < c)
                { k=a; l=c; m=d; n=b; }
                else
                { k=c; l=a; m=d; n=b; }
            else
                if (a < b)
                { k=c; l=d; m=a; n=b; }
                else
                { k=c; l=d; m=b; n=a; }
    else
        if (b < c)
            if (b < d)
                if (a < d)
                    if (a < b)
                    { k=a; l=b; m=d; n=c; }
                    else
                    { k=b; l=a; m=d; n=c; }
                else
                    if (a < c)
                    { k=b; l=d; m=a; n=c; }
                    else
                    { k=b; l=d; m=c; n=a; }
            else
                if (a < b)
                    if (a < d)
                    { k=a; l=d; m=b; n=c; }
                    else
                    { k=d; l=a; m=b; n=c; }
                else
                    if (a < c)
                    { k=d; l=b; m=a; n=c; }
                    else
                    { k=d; l=b; m=c; n=a; }
        else
            if (a < c)
                if (a < d)
                { k=a; l=d; m=c; n=b; }
                else
                { k=d; l=a; m=c; n=b; }
            else
                if (a < b)
                { k=d; l=c; m=a; n=b; }
                else
                { k=d; l=c; m=b; n=a; }
}

/** In-Place-Sort-3 `a`, `b` and `c`.
 * Time-Complexity: 3 CMP
 * Code Complexity: 1 CMP, 3 MOV
 */
void ip_sort(T)(ref T a, ref T b, ref T c)
{
    import std.algorithm : swap;
    if (b < c)
        if (a < c)
            if (a < b)
            { /* already sorted */ }
            else
            { swap(a,b); }
        else
        { perm3_231(a,b,c); }
    else
        if (a < b)
            if (a < c)
            { swap(b,c); }
            else
            { perm3_312(a,b,c); }
        else
        { swap(a,c); }
}

/** In-Place-Sort-4 `a`, `b`, `c` and `d`.
 *        a b c d
 * TODO: FIXME: Fails for input { 1, 3, 4, 2 }
 */
void ip_sort(T)(ref T a, ref T b, ref T c, ref T d)
{
    import std.algorithm : swap;
    if (c < d)
        if (b < d)
            if (b < c)
                if (a < c)
                    if (a < b)
                    { /* already sorted */ }
                    else
                    { swap(a,b); }
                else
                    if (a < d)
                    { perm3_231(a,b,c); }
                    else
                    { perm4_2341(a,b,c,d); }
            else
                if (a < b)
                    if (a < c)
                    { swap(b,c); }
                    else
                    { perm3_312(a,b,c); }
                else
                    if (a < d)
                    { swap(a,c); }
                    else
                    { perm3_231(a,c,d); }
        else
            if (a < d)
                if (a < c)
                { perm3_231(b,c,d); }
                else
                { perm4_3142(a,b,c,d); }
            else
                if (a < b)
                { perm4_3412(a,b,c,d); }
                else
                { perm4_3421(a,b,c,d); }
    else
        if (b < c)
            if (b < d)
                if (a < d)
                    if (a < b)
                    { swap(c,d); }
                    else
                    { perm4_2143(a,b,c,d); }
                else
                    if (a < c)
                    { perm4_2413(a,b,c,d); }
                    else
                    { perm3_231(a,b,d); }
            else
                if (a < b)
                    if (a < d)
                    { perm3_312(b,c,d); }
                    else
                    { perm4_4123(a,b,c,d); }
                else
                    if (a < c)
                    { perm3_312(a,c,d); }
                    else
                    { swap(a,d); }
        else
            if (a < c)
                if (a < d)
                { swap(b,d); }
                else
                { perm3_312(a,b,d); }
            else
                if (a < b)
                { perm4_4312(a,b,c,d); }
                else
                { perm4_4321(a,b,c,d); }
}

/** \em In-Place Sort the array at `a` of length `n`.
 * Returns: `true` if sort was possible, `false` otherwise.
 */
bool ip_sort(T)(T* a, size_t n)
{
    bool rval = false;
    switch (n)
    {
    case 0:
    case 1: /* do nothing */ rval = true; break;
    case 2: ip_sort(a[0], a[1]); rval = true; break;
    case 3: ip_sort(a[0], a[1], a[2]); rval = true; break;
    case 4: ip_sort(a[0], a[1], a[2], a[3]); rval = true; break;
    }
    return rval;
}
