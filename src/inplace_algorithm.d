module inplace_algorithm;

import std.functional : unaryFun;

import typecons_ex : hasIndexing;

version(unittest)
{
    import std.algorithm.comparison : equal;
    import dbgio : dln;
}

/** Returns: `r` eagerly in-place filtered on `predicate`. */
C filteredInplace(alias predicate, C)(C r) @safe
    if (is(typeof(unaryFun!predicate)) &&
        hasIndexing!C)
{
    import std.traits : hasElaborateDestructor, isMutable, hasIndirections;
    import std.range.primitives : ElementType;
    import std.algorithm.mutation : move;
    import traits_ex : ownsItsElements;

    alias pred = unaryFun!predicate;
    alias E = ElementType!C;

    size_t dstIx = 0;           // destination index

    // skip leading passing elements
    // TODO reuse .indexOf!(_ => !pred(_)) algorithm in `Array`
    while (dstIx < r.length && pred(r[dstIx]))
    {
        dstIx += 1;
    }

    // inline filtering
    foreach (immutable srcIx; dstIx + 1 .. r.length)
    {
        // TODO move this into unchecked function in Array
        if (pred(r[srcIx]))
        {
            static if (isMutable!E &&
                       !hasIndirections!E)
            {
                move(r[srcIx], r[dstIx]); // TODO reuse function in array
            }
            else static if (ownsItsElements!C)
            {
                move(r[srcIx], r[dstIx]); // TODO reuse function in array
            }
            else
            {
                static assert(false, "Cannot move elements in instance of " ~ C.stringof);
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

    r.length = dstIx;           // truncate

    return move(r);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    import unique_range : intoUniqueRange;
    import array_ex : UncopyableArray, SortedSetUncopyableArray;

    alias E = int;
    foreach (C; AliasSeq!(UncopyableArray// , TODO SortedSetUncopyableArray
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

        // odd elements
        immutable E[6] c1 = [3, 11, 13, 15, 17, 19];
        assert(A.withElements(3, 11, 12, 13, 14, 15, 16, 17, 18, 19)
                .filteredInplace!(_ => _ & 1)
                .intoUniqueRange()
                .equal(c1[]));

        // elements less than or equal to limit
        immutable E[6] c2 = [3, 11, 12, 13, 14, 15];
        assert(A.withElements(3, 11, 12, 13, 14, 15, 16, 17, 18, 19)
                .filteredInplace!(_ => _ <= 15)
                .intoUniqueRange()
                .equal(c2[]));
    }
}
