module inplace_algorithm;

import std.functional : unaryFun;

import container_traits : isSet;
import typecons_ex : hasIndexing;

version(unittest)
{
    import std.algorithm.comparison : equal;
    import dbgio : dln;
}

/** Returns: `r` eagerly in-place filtered on `predicate`.
    TODO Move to free function in array_ex.d to get @trusted access to private Array._mptr
 */
C filteredInplace(alias predicate, C)(C r) @trusted // TODO remove @trusted
    if (is(typeof(unaryFun!predicate)) &&
        hasIndexing!C)          // TODO extend to `isArrayContainer`!C eller `isRandomAccessContainer!C`
{
    import std.typecons : Unqual;
    import std.traits : hasElaborateDestructor, isMutable, hasIndirections;
    import std.range.primitives : ElementType;
    import std.algorithm.mutation : move;
    import traits_ex : ownsItsElements;

    alias pred = unaryFun!predicate;
    alias E = ElementType!C;
    alias MutableC = Unqual!C;

    static if (__traits(hasMember, r, `ptr`))
    {
        size_t dstIx = 0;           // destination index

        // skip leading passing elements
        // TODO reuse .indexOf!(_ => !pred(_)) algorithm in `Array`
        while (dstIx < r.length && pred(r.ptr[dstIx]))
        {
            dstIx += 1;
        }

        // inline filtering
        foreach (immutable srcIx; dstIx + 1 .. r.length)
        {
            // TODO move this into @trusted member of Array
            if (pred(r.ptr[srcIx]))
            {
                static if (isMutable!E &&
                           !hasIndirections!E)
                {
                    move(r.ptr[srcIx], r.ptr[dstIx]); // TODO reuse function in array
                }
                else static if (ownsItsElements!C)
                {
                    move(r.ptr[srcIx], r.ptr[dstIx]); // TODO reuse function in array
                }
                else
                {
                    static assert(0, `Cannot move elements in instance of ` ~ C.stringof);
                }
                dstIx += 1;
            }
            else
            {
                static if (hasElaborateDestructor!E)
                {
                    .destroy(e);
                }
            }
        }

        static if (__traits(hasMember, C, `shrinkTo`))
        {
            r.shrinkTo(dstIx);  // length will all always shrink
        }
        else
        {
            r.length = dstIx;
        }
    }

    return move(r);             // TODO remove move when compiler does it for us
}

/** Returns: `r` eagerly in-place filtered on `predicate`.
 */
C filteredInplace(alias predicate, C)(C r)
    if (is(typeof(unaryFun!predicate(C.ElementType.init))) &&
        isSet!C)
{
    import std.algorithm.mutation : move;
    static if (__traits(hasMember, C, "remove"))
    {
        import std.functional : not;
        import hashset : filtered;
        return move(r).filtered!predicate(); // TODO remove move when compiler does it for us
    }
    else
    {
        C s;
        import std.algorithm.iteration : filter;
        foreach (e; r[].filter!predicate)
        {
            s.insert(e);
        }
        return move(s);             // TODO remove move when compiler does it for us
    }
}

/// inplace filtering on hashset
@safe pure nothrow @nogc unittest
{
    import std.algorithm.iteration : filter;
    import hashset : HashSet, byElement;
    import digestx.fnv : FNV;

    alias X = HashSet!(uint, null, FNV!(64, true));
    enum pred = "a != 11";
    // alias pred = (_) => _ != 1;

    auto x = X.withElements([11, 12].s).filteredInplace!pred.byElement;
    assert(x.front == 12);

    x.popFront();
    assert(x.empty);
}

/** Fyilter `r` eagerly in-place using `predicate`. */
void filterInplace(alias predicate, C)(ref C r) @trusted // TODO remove @trusted
    if (is(typeof(unaryFun!predicate)) &&
        hasIndexing!C)          // TODO extend to `isArrayContainer`!C eller `isRandomAccessContainer!C`
{
    import std.algorithm.mutation : move;
    r = move(r).filteredInplace!predicate(); // TODO remove move when compiler does it for us
}

@safe pure nothrow @nogc unittest
{
    import std.algorithm.mutation : move;
    import std.meta : AliasSeq;
    import unique_range : intoUniqueRange;
    import basic_array : BasicArray;
    import array_ex : SortedSetUniqueArray;

    alias E = int;
    foreach (C; AliasSeq!(BasicArray// ,
                          // TODO SortedSetUniqueArray
                 ))
    {
        alias A = C!E;

        static assert(is(A == typeof(A().filteredInplace!(_ => _ & 1))));

        // empty case
        immutable E[0] c0 = [];
        assert(A.withCapacity(0)
                .filteredInplace!(_ => _ & 1)
                .intoUniqueRange()
                .equal(c0[]));

        // few elements triggers small-array optimization
        immutable E[2] c2 = [3, 11];
        auto a2 = A([2, 3, 11, 12].s);
        assert(move(a2).filteredInplace!(_ => _ & 1)
                       .intoUniqueRange()
                       .equal(c2[]));

        // odd elements
        immutable E[6] c6 = [3, 11, 13, 15, 17, 19];
        auto a6 = A([3, 11, 12, 13, 14, 15, 16, 17, 18, 19].s);
        assert(move(a6).filteredInplace!(_ => _ & 1)
                       .intoUniqueRange()
                       .equal(c6[]));

        // elements less than or equal to limit
        immutable E[7] c7 = [3, 11, 12, 13, 14, 15, 16];
        auto a7 = A([3, 11, 12, 13, 14, 15, 16, 17, 18, 19].s
            );
        assert(move(a7).filteredInplace!(_ => _ <= 16)
                       .intoUniqueRange()
                       .equal(c7[]));

        auto a3 = A([2, 4, 11].s);
        a3.filterInplace!(_ => _ & 1);
        assert(a3.length == 1);
        assert(a3[0] == 11);
    }

}

version(unittest)
{
    import array_help : s;
}
