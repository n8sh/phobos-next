void main()
{
    // standard storage
    import std.traits : hasMember;
    import std.range : iota;
    import std.array : array, Appender;
    import std.random : randomShuffle;
    import std.container.rbtree : RedBlackTree;
    import std.algorithm.searching : minElement, maxElement;
    import std.typecons : Nullable;

    import nxt.array_help : toUbytes;
    import nxt.pure_mallocator : Mallocator = PureMallocator;
    import nxt.open_hashmap_or_hashset : OpenHashMap, OpenHashSet, defaultKeyEqualPredOf;
    import nxt.hash_functions : lemireHash64;
    import nxt.trie : RadixTreeSet;

    import std.stdio : write, writeln, writef, writefln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;

    import std.conv : to;

    immutable elementCount = 400_000; ///< Number of elements.
    immutable runCount = 10;          ///< Number of runs per benchmark.

    auto testSource = iota(0, elementCount).array;
    const useRandomShuffledSource = true;
    if (useRandomShuffledSource)
    {
        randomShuffle(testSource);
    }

    writefln("\nElement count: %s", elementCount);
    writefln("\nRun count: %s", runCount);

    writefln("\nArrays:\n");

    alias Sample = ulong;

    writefln("\nSets:\n");

    foreach (A; AliasSeq!(OpenHashSet!(Nullable!(ulong, ulong.max), lemireHash64)))
    {
        // scope

        A a = makeWithTriedCapacity!(A)(elementCount);

        // TODO const testSource = iotaArrayOf!(0, A.ElementType)(elementCount);

        writef("- ");

        {
            immutable startTime = MonoTime.currTime();
            foreach (immutable i; testSource)
            {
                static if (hasMember!(A, `ElementType`) &&
                           is(A.ElementType == ubyte[]))
                {
                    a.insert(i.toUbytes);
                }
                else
                {
                    static if (hasMember!(A, `ElementType`))
                    {
                        const element = A.ElementType(i);
                    }
                    else
                    {
                        const element = i;
                    }
                    a.insert(element);
                }
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - startTime).total!"nsecs" / elementCount);
        }

        {
            immutable startTime = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (immutable i; testSource)
            {
                static if (hasMember!(A, `ElementType`) &&
                           is(A.ElementType == ubyte[]))
                {
                    hitCount += a.contains(i.toUbytes);
                }
                else
                {
                    static if (hasMember!(A, `ElementType`))
                    {
                        const element = A.ElementType(i); // wrap in `i` in `Nullable`
                    }
                    else
                    {
                        const element = i;
                    }
                    static if (hasMember!(A, "contains"))
                    {
                        hitCount += a.contains(element);
                    }
                    else
                    {
                        hitCount += element in a;
                    }
                }
            }
            const ok = hitCount == elementCount; // for side effect in output
            immutable after = MonoTime.currTime();
            writef(", contains: %3.1f ns/op (%s)", cast(double)(after - startTime).total!"nsecs" / elementCount, ok ? "OK" : "ERR");
        }

        /* NOTE I couldn't make this faster so skiping */
        /* static if (hasMember!(A, "containsUsingLinearSearch")) */
        /* { */
        /*     { */
        /*         immutable startTime = MonoTime.currTime(); */
        /*         size_t hitCount = 0; */
        /*         import std.algorithm.comparison : min; */
        /*         const testSourceCount = min(100, testSource.length); // reduce to 1000 tests for now because of slow linear search */
        /*         foreach (immutable i; testSource[0 .. testSourceCount]) */
        /*         { */
        /*             static if (hasMember!(A, `ElementType`)) */
        /*             { */
        /*                 const element = A.ElementType(i); // wrap in i in Nullable */
        /*             } */
        /*             else */
        /*             { */
        /*                 const element = i; */
        /*             } */
        /*             hitCount += a.containsUsingLinearSearch(element); */
        /*         } */
        /*         const ok = hitCount == testSourceCount; // for side effect in output */
        /*         immutable after = MonoTime.currTime(); */
        /*         writef(", containsUsingLinearSearch: %3.1f ns/op (%s)", cast(double)(after - startTime).total!"nsecs" / testSourceCount, ok ? "OK" : "ERR"); */
        /*     } */
        /* } */

        static if (hasMember!(A, `withCapacity`))
        {
            A b = A.withCapacity(elementCount);
            scope spans_ns = new double[runCount];
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    static if (hasMember!(A, `ElementType`) &&
                               is(A.ElementType == ubyte[]))
                    {
                        b.insert(i.toUbytes);
                    }
                    else
                    {
                        static if (hasMember!(A, `ElementType`))
                        {
                            const element = A.ElementType(i); // wrap in `i` in `Nullable`
                        }
                        else
                        {
                            const element = i;
                        }
                        b.insert(element);
                    }
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", insert (no growth): %3.1f ns/op",
                   minElement(spans_ns[]) / elementCount);
        }

        writef(` for %s`, A.stringof);

        static if (hasMember!(A, `binCounts`))
        {
            writef(" %s", a.binCounts());
        }
        static if (hasMember!(A, `smallBinCapacity`))
        {
            writef(" smallBinCapacity:%s", A.smallBinCapacity);
        }
        static if (hasMember!(A, `averageProbeCount`))
        {
            writef(" averageProbeCount:%s", a.averageProbeCount);
        }

        writeln();

        static if (hasMember!(A, `clear`))
        {
            a.clear();
        }
    }

    writefln("\nMaps:\n");

    foreach (A; AliasSeq!(OpenHashMap!(Nullable!(ulong, ulong.max), ulong, lemireHash64)))
    {
        A a = makeWithTriedCapacity!(A)(elementCount);

        writef("- ");

        // allocate
        const keys = iotaArrayOf!(A.KeyType)(0, elementCount);

        {
            scope spans_ns = new double[runCount];
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    const element = A.ElementType(keys[i], A.ValueType.init);
                    a.insert(element);
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef("insert (w growth): %3.1f ns/op",
                   minElement(spans_ns[]) / elementCount);
        }

        {
            bool okAll = true;
            scope spans_ns = new double[runCount];
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                size_t hitCount = 0;
                foreach (immutable i; testSource)
                {
                    hitCount += a.contains(keys[i]);
                }
                const ok = hitCount == elementCount; // for side effect in output
                if (!ok) { okAll = false; }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", contains: %3.1f ns/op (%s)",
                   minElement(spans_ns[]) / elementCount,
                   okAll ? "OK" : "ERR");
        }

        {
            bool okAll = true;
            scope spans_ns = new double[runCount];
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                size_t hitCount = 0;
                foreach (immutable i; testSource)
                {
                    hitCount += cast(bool)(keys[i] in a);
                }
                const ok = hitCount == elementCount; // for side effect in output
                if (!ok) { okAll = false; }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", in: %3.1f ns/op (%s)",
                   minElement(spans_ns[]) / elementCount,
                   okAll ? "OK" : "ERR");
        }

        {
            scope spans_ns = new double[runCount];
            foreach (const runIx; 0 .. runCount)
            {
                A b = A.withCapacity(elementCount);
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    b.insert(A.ElementType(keys[i], A.ValueType.init));
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", insert (no growth): %3.1f ns/op", minElement(spans_ns[]) / elementCount);
        }

        writef(` for %s`, A.stringof);

        static if (hasMember!(A, `binCounts`))
        {
            writef(" %s", a.binCounts());
        }
        static if (hasMember!(A, `smallBinCapacity`))
        {
            writef(" smallBinCapacity:%s", A.smallBinCapacity);
        }
        static if (hasMember!(A, `totalProbeCount`))
        {
            writef(" averageProbeCount:%s", cast(double)a.totalProbeCount/a.length);
        }

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }
}

import std.traits : isDynamicArray;

auto makeWithTriedCapacity(A)(size_t elementCount)
if (is(A == class) ||
    is(A == struct) ||
    isDynamicArray!A)
{
    import std.traits : hasMember;
    static if (hasMember!(A, `withCapacity`))
    {
        return A.withCapacity(elementCount);
    }
    else static if (hasMember!(A, `reserve`))
    {
        static if (is(A == class))
        {
            A a = new A();
        }
        else
        {
            A a;
        }
        static if (hasMember!(A, `reserve`) &&
                   __traits(compiles, { a.reserve(elementCount); }))
        {
            a.reserve(elementCount);
        }
        else static if (hasMember!(A, `reserve`) &&
                        __traits(compiles, { a.reserve!uint(elementCount); }))
        {
            a.reserve!uint(elementCount);
        }
        return a;
    }
    else static if (is(A == class))
    {
        return new A();
    }
    else static if (isDynamicArray!A)
    {
        import std.range.primitives : ElementType;
        return new ElementType!A[elementCount];
    }
    else static if (is(A == struct))
    {
        return A();
    }
    else
    {
        static assert(false, "Unsupported type `" ~ A.stringof ~ "`");
    }
}

private T[] iotaArrayOf(T, U)(U begin, U end)
{
    typeof(return) es = new T[end];
    foreach (immutable i; begin .. end)
    {
        static if (is(typeof(T(i)))) // if possible
        {
            es[i] = T(i);       // try normal construction
        }
        else
        {
            import nxt.sso_string : SSOString;
            import std.conv : to;
            static if (is(T == SSOString))
            {
                es[i] = T(i.to!string);     // otherwise conv which may allocate
            }
            else
            {
                es[i] = i.to!T;     // otherwise conv which may allocate
            }
        }
    }
    return es;
}
