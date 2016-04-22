#!/usr/bin/env rdmd-dev-module

/** Extensions to std.algorithm.sort.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
*/

module sort_ex;

import std.traits: isAggregateType;
import std.range: ElementType, isRandomAccessRange, isInputRange;

private template xtorFun(alias xtor)
{
    import std.traits: isIntegral;
    static if (is(typeof(xtor) : string))
    {
        auto ref xtorFun(T)(auto ref T a)
        {
            mixin("with (a) { return " ~ xtor ~ "; }");
        }
    }
    else static if (isIntegral!(typeof(xtor)))
    {
        auto ref xtorFun(T)(auto ref T a)
        {
            import std.conv: to;
            mixin("return a.tupleof[" ~ xtor.to!string ~ "];");
        }
    }
    else
    {
        alias xtorFun = xtor;
    }
}

/* private alias makePredicate(alias xtor) = (a, b) => (xtorFun!xtor(a) < xtorFun!xtor(b)); */

/* auto sortBy(xtors..., R)(R r) { */
/*     alias preds = staticMap!(makePredicate, xtors); */
/*     return r.sort!preds; */
/* } */

/** Sort Random Access Range $(D R) of Aggregates on Value of Calls to $(D xtor).
    See also: http://forum.dlang.org/thread/nqwzojnlidlsmpunpqqy@forum.dlang.org#post-dmfvkbfhzigecnwglrur:40forum.dlang.org
 */
void sortBy(alias xtor, R)(R r) if (isRandomAccessRange!R &&
                                    isAggregateType!(ElementType!R))
{
    import std.algorithm : sort;
    import std.functional: unaryFun;
    r.sort!((a, b) => (xtorFun!xtor(a) <
                       xtorFun!xtor(b)));
}

/** Reverse Sort Random Access Range $(D R) of Aggregates on Value of Calls to $(D xtor).
    See also: http://forum.dlang.org/thread/nqwzojnlidlsmpunpqqy@forum.dlang.org#post-dmfvkbfhzigecnwglrur:40forum.dlang.org
*/
void rsortBy(alias xtor, R)(R r) if (isRandomAccessRange!R &&
                                     isAggregateType!(ElementType!R))
{
    import std.algorithm : sort;
    import std.functional: unaryFun;
    r.sort!((a, b) => (xtorFun!xtor(a) >
                       xtorFun!xtor(b)));
}

@safe pure nothrow unittest
{
    static struct X { int x, y, z; }

    auto r = [ X(1, 2, 1),
               X(0, 1, 2),
               X(2, 0, 0) ];

    r.sortBy!(a => a.x);
    assert(r == [ X(0, 1, 2),
                  X(1, 2, 1),
                  X(2, 0, 0) ]);
    r.sortBy!(a => a.y);
    assert(r == [ X(2, 0, 0),
                  X(0, 1, 2),
                  X(1, 2, 1)] );
    r.sortBy!(a => a.z);
    assert(r == [ X(2, 0, 0),
                  X(1, 2, 1),
                  X(0, 1, 2) ]);

    r.sortBy!"x";
    assert(r == [ X(0, 1, 2),
                  X(1, 2, 1),
                  X(2, 0, 0) ]);
    r.sortBy!"y";
    assert(r == [ X(2, 0, 0),
                  X(0, 1, 2),
                  X(1, 2, 1)] );
    r.sortBy!"z";
    assert(r == [ X(2, 0, 0),
                  X(1, 2, 1),
                  X(0, 1, 2) ]);

    r.sortBy!0;
    assert(r == [ X(0, 1, 2),
                  X(1, 2, 1),
                  X(2, 0, 0) ]);
    r.sortBy!1;
    assert(r == [ X(2, 0, 0),
                  X(0, 1, 2),
                  X(1, 2, 1)] );
    r.sortBy!2;
    assert(r == [ X(2, 0, 0),
                  X(1, 2, 1),
                  X(0, 1, 2) ]);
}

/** Returns: $(D r) sorted.
    If needed a GC-copy of $(D r) is allocated, sorted and returned.
    See also: http://forum.dlang.org/thread/tnrvudehinmkvbifovwo@forum.dlang.org#post-tnrvudehinmkvbifovwo:40forum.dlang.org
    TODO Add to Phobos
*/
auto sorted(R, E = ElementType!R)(R r)
{
    import std.traits : isNarrowString;
    import std.range: hasLength;
    import range_ex : isSortedRange;

    static if (isSortedRange!R)
    {
        return r;
    }
    else
    {
        static if (isRandomAccessRange!R)
        {
            auto s = r.dup;
        }
        else static if (isNarrowString!R)
        {
            import std.conv : to;
            auto s = r.to!(dchar[]); // need dchar for random access
        }
        else static if (hasLength!R)
        {
            import std.algorithm: copy;
            auto s = new E[r.length];
            static if (is(typeof(r[]))) // TODO unpretty
            {
                r[].copy(s);
            }
            else
            {
                r.copy(s);
            }
        }
        else
        {
            E[] s; // TODO use Appender?
            foreach (const ref e; r[])
            {
                s ~= e;             // TODO optimize?
            }
        }

        import std.algorithm.sorting : sort;
        return sort(s);
    }
}

version(unittest) import std.algorithm.comparison : equal;

///
@safe pure unittest
{
    assert(equal("öaA".sorted, "Aaö"));
    assert(equal("öaA"w.sorted, "Aaö"));
    assert(equal("öaA"d.sorted, "Aaö"));
}

///
@safe pure unittest
{
    import std.algorithm.sorting : sort;
    auto x = "öaA"d;
    auto y = sort(x.dup).sorted; // parameter to sorted is a SortedRange
    assert(equal(y, "Aaö"));
}

///
@safe pure unittest
{
    import std.algorithm.sorting : sort;
    immutable x = [3, 2, 1];
    auto y = x.dup;
    sort(y);
    assert(equal(x.sorted, y));
}

///
unittest
{
    import std.container: Array;
    auto x = Array!int(3, 2, 1);
    assert(equal(x.sorted, [1, 2, 3]));
}

///
unittest
{
    import std.container: SList;
    auto x = SList!int(3, 2, 1);
    assert(equal(x.sorted, [1, 2, 3]));
}

import std.random : Random;

/** Functional version of `std.random.randomShuffle`.

    Returns: $(D r) randomly shuffled.

    If needed a GC-copy of $(D r) is allocated, sorted and returned.
*/
auto randomlyShuffled(Range, RandomGen)(Range r, ref RandomGen gen)
{
    import std.random : randomShuffle;
    r.randomShuffle(gen);
    // TODO reuse copying logic in `sorted`
    return r;
}

///
auto randomlyShuffled(Range)(Range r)
{
    import std.random : randomShuffle;
    r.randomShuffle();
    // TODO reuse copying logic in `sorted`
    return r;
}

///
@safe pure unittest
{
    immutable x = [3, 2, 1];
    auto y = x.dup;
    Random random;
    y.randomlyShuffled(random);
}

// /** Assign-Sort-4 \p a, \p b, \p c and \p d into \p k, \p l, \p m and \p n .
//  * \complexity[time] (\em Small!): 5 CMP, 4 MOV
//  * \complexity[code] (\em Large!): 23 CMP, 24*4 MOV
//  * \note Uses value-semantic on input arguments to enable move-semantics.
//  */
// void asort4(T)(T a, T b, T c, T d,
//                ref T k, ref T l, ref T m, ref T n)
// {
//     if (c < d)
//         if (b < d)
//             if (b < c)
//                 if (a < c)
//                     if (a < b)
//                     { k=a; l=b; m=c; n=d; }
//                     else
//                     { k=b; l=a; m=c; n=d; }
//                 else
//                     if (a < d)
//                     { k=b; l=c; m=a; n=d; }
//                     else
//                     { k=b; l=c; m=d; n=a; }
//             else
//                 if (a < b)
//                     if (a < c)
//                     { k=a; l=c; m=b; n=d; }
//                     else
//                     { k=c; l=a; m=b; n=d; }
//                 else
//                     if (a < d)
//                     { k=c; l=b; m=a; n=d; }
//                     else
//                     { k=c; l=b; m=d; n=a; }
//         else
//             if (a < d)
//                 if (a < c)
//                 { k=a; l=c; m=d; n=b; }
//                 else
//                 { k=c; l=a; m=d; n=b; }
//             else
//                 if (a < b)
//                 { k=c; l=d; m=a; n=b; }
//                 else
//                 { k=c; l=d; m=b; n=a; }
//     else
//         if (b < c)
//             if (b < d)
//                 if (a < d)
//                     if (a < b)
//                     { k=a; l=b; m=d; n=c; }
//                     else
//                     { k=b; l=a; m=d; n=c; }
//                 else
//                     if (a < c)
//                     { k=b; l=d; m=a; n=c; }
//                     else
//                     { k=b; l=d; m=c; n=a; }
//             else
//                 if (a < b)
//                     if (a < d)
//                     { k=a; l=d; m=b; n=c; }
//                     else
//                     { k=d; l=a; m=b; n=c; }
//                 else
//                     if (a < c)
//                     { k=d; l=b; m=a; n=c; }
//                     else
//                     { k=d; l=b; m=c; n=a; }
//         else
//             if (a < c)
//                 if (a < d)
//                 { k=a; l=d; m=c; n=b; }
//                 else
//                 { k=d; l=a; m=c; n=b; }
//             else
//                 if (a < b)
//                 { k=d; l=c; m=a; n=b; }
//                 else
//                 { k=d; l=c; m=b; n=a; }
// }

// /*! In-Place-Sort-3 \p a, \p b and \p c.
//  * \complexity[time]: 3 CMP
//  * \complexity[code]: 1 CMP, 3 MOV
//  */
// void ip_sort(T)(ref T a, ref T b, ref T c)
// {
//     import std.algorithm : swap;
//     if (b < c)
//         if (a < c)
//             if (a < b)
//             { /* already sorted */ }
//             else
//             { swap(a,b); }
//         else
//         { perm3_231(a,b,c); }
//     else
//         if (a < b)
//             if (a < c)
//             { swap(b,c); }
//             else
//             { perm3_312(a,b,c); }
//         else
//         { swap(a,c); }
// }

// /*! In-Place-Sort-4 \p a, \p b, \p c and \p d.
//  *        a b c d
//  * TODO: FIXME: Fails for input { 1, 3, 4, 2 }
//  */
// void ip_sort(T)(ref T a, ref T b, ref T c, ref T d)
// {
//     import std.algorithm : swap;
//     if (c < d)
//         if (b < d)
//             if (b < c)
//                 if (a < c)
//                     if (a < b)
//                     { /* already sorted */ }
//                     else
//                     { swap(a,b); }
//                 else
//                     if (a < d)
//                     { perm3_231(a,b,c); }
//                     else
//                     { perm4_2341(a,b,c,d); }
//             else
//                 if (a < b)
//                     if (a < c)
//                     { swap(b,c); }
//                     else
//                     { perm3_312(a,b,c); }
//                 else
//                     if (a < d)
//                     { swap(a,c); }
//                     else
//                     { perm3_231(a,c,d); }
//         else
//             if (a < d)
//                 if (a < c)
//                 { perm3_231(b,c,d); }
//                 else
//                 { perm4_3142(a,b,c,d); }
//             else
//                 if (a < b)
//                 { perm4_3412(a,b,c,d); }
//                 else
//                 { perm4_3421(a,b,c,d); }
//     else
//         if (b < c)
//             if (b < d)
//                 if (a < d)
//                     if (a < b)
//                     { swap(c,d); }
//                     else
//                     { perm4_2143(a,b,c,d); }
//                 else
//                     if (a < c)
//                     { perm4_2413(a,b,c,d); }
//                     else
//                     { perm3_231(a,b,d); }
//             else
//                 if (a < b)
//                     if (a < d)
//                     { perm3_312(b,c,d); }
//                     else
//                     { perm4_4123(a,b,c,d); }
//                 else
//                     if (a < c)
//                     { perm3_312(a,c,d); }
//                     else
//                     { swap(a,d); }
//         else
//             if (a < c)
//                 if (a < d)
//                 { swap(b,d); }
//                 else
//                 { perm3_312(a,b,d); }
//             else
//                 if (a < b)
//                 { perm4_4312(a,b,c,d); }
//                 else
//                 { perm4_4321(a,b,c,d); }
// }

// /*! \em In-Place Sort the array at \p a of length \p n.
//  * \return \c true if sort was possible, \c false otherwise.
//  */
// bool ip_sort(T)(T* a, size_t n)
// {
//     bool rval = false;
//     switch (n)
//     {
//     case 0:
//     case 1: /* do nothing */ rval = true; break;
//     case 2: ip_sort(a[0], a[1]); rval = true; break;
//     case 3: ip_sort(a[0], a[1], a[2]); rval = true; break;
//     case 4: ip_sort(a[0], a[1], a[2], a[3]); rval = true; break;
//     }
//     return rval;
// }
