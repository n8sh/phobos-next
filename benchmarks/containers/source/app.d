void main()
{
    // standard storage
    import std.traits : hasMember;
    import std.range : iota;
    import std.array : array, Appender;
    import std.random : randomShuffle;
    import std.container.array : StdArray = Array;
    import std.container.rbtree : RedBlackTree;
    import std.algorithm.searching : minElement, maxElement;

    // my containers
    import nxt.dynamic_array : DynamicArray;
    import nxt.array_help : toUbytes;
    import nxt.variant_arrays : VariantArrays;
    // import nxt.sso_hashmap_or_hashset : SSOHashSet, SSOHashMap;
    import nxt.open_hashmap_or_hashset : OpenHashMap, OpenHashSet;
    import nxt.sso_string : SSOString;
    import nxt.address : Address;

    import std.digest.murmurhash : MurmurHash3;
    import nxt.xxhash64 : XXHash64;
    import nxt.hash_functions;
    import nxt.digestx.fnv : FNV;

    import nxt.filters : DenseSetFilter;
    import nxt.filterarray : DenseSetFilterGrowableArray;

    import std.typecons : Nullable;
    import nxt.trie : RadixTreeSetGrowOnly;

    import std.stdio : write, writeln, writef, writefln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;

    import std.conv : to;

    immutable elementCount = 400_000;
    immutable runCount = 3;     ///< Number of runs per benchmark.

    auto testSource = iota(0, elementCount).array;
    const useRandomShuffledSource = true;
    if (useRandomShuffledSource)
    {
        randomShuffle(testSource);
    }

    writefln("\nElement count: %s", elementCount);

    writefln("\nArrays:\n");

    foreach (A; AliasSeq!(DynamicArray!uint,
                          VariantArrays!uint,
                          StdArray!uint,
                          Appender!(uint[]),
                          uint[]))
    {
        writef("- ");

        static if (hasMember!(A, `withCapacity`))
        {
            auto a = A.withCapacity(elementCount);
        }
        else static if (hasMember!(A, `reserve`))
        {
            A a;
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
        }
        else
        {
            A a;
        }

        {
            auto spans_ns = DynamicArray!double.withLength(runCount);
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    a ~= i.to!uint;     // need to cast away const here for now. TODO remove this requirement
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef("Appended: %3.1f ns/op",
                   minElement(spans_ns[]) / elementCount);
        }

        writefln(` for %s`, A.stringof);

        static if (hasMember!(A, `clear`))
        {
            a.clear();
        }
    }

    writefln("\nSets:\n");

    foreach (A; AliasSeq!(DenseSetFilter!(uint),
                          DenseSetFilterGrowableArray!(uint),

                          // functions
                          // SSOHashSet!(uint, null, identityHash64Of),
                          // SSOHashSet!(uint, null, typeidHashOf),
                          // SSOHashSet!(uint, null, hashOf),

                          // SSOHashSet!(uint, null, muellerHash64),
                          // SSOHashSet!(uint, null, wangMixHash64),
                          // SSOHashSet!(uint, null, FNV!(64, true)),

                          // std.digests
                          // SSOHashSet!(uint, null, MurmurHash3!(128)),
                          // SSOHashSet!(uint, null, XXHash64),

                          OpenHashSet!(Nullable!(uint, uint.max)),
                          OpenHashSet!(Nullable!(uint, uint.max), FNV!(64, true)),

                          RadixTreeSetGrowOnly!(uint),
                          RedBlackTree!(uint),

                          // SSOHashSet!(ulong, null, wangMixHash64),
                          // SSOHashSet!(ulong, null, muellerHash64),
                          // SSOHashSet!(ulong, null, FNV!(64, true), 2),
                          // SSOHashSet!(ulong, null, FNV!(64, true), 3),
                          // SSOHashSet!(ulong, null, FNV!(64, true), 4),

                          OpenHashSet!(Nullable!(ulong, ulong.max)),
                          OpenHashSet!(Nullable!(ulong, ulong.max), FNV!(64, true)),
                          OpenHashSet!(Nullable!(ulong, ulong.max), wangMixHash64),
                          OpenHashSet!(Address),
                          // TODO OpenHashSet!(ulong*, FNV!(64, true)),

                          RadixTreeSetGrowOnly!(ulong),
                          RedBlackTree!(ulong),

                          // TODO OpenHashSet!(string, FNV!(64, true)),
                          // TODO OpenHashSet!(string, wangMixHash64),
                 ))
    {
        // scope
        static if (is(A == class))
        {
            auto a = new A();
        }
        else
        {
            auto a = A();
        }

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
                        static if (is(A.ElementType == Address))
                        {
                            const element = A.ElementType(i + 1); ///< Start at 1 instead of 0 because `Address` uses 0 for `nullValue`.
                        }
                        else
                        {
                            const element = A.ElementType(i); // wrap in `i` in `Nullable`
                        }
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
                        static if (is(A.ElementType == Address))
                        {
                            const element = A.ElementType(i + 1); ///< Start at 1 instead of 0 because `Address` uses 0 for `nullValue`.
                        }
                        else
                        {
                            const element = A.ElementType(i); // wrap in `i` in `Nullable`
                        }
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
            auto spans_ns = DynamicArray!double.withLength(runCount);
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
                            static if (is(A.ElementType == Address))
                            {
                                const element = A.ElementType(i + 1); ///< Start at 1 instead of 0 because `Address` uses 0 for `nullValue`.
                            }
                            else
                            {
                                const element = A.ElementType(i); // wrap in `i` in `Nullable`
                            }
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

    foreach (A; AliasSeq!(

                 // uint => uint
                 // SSOHashMap!(uint, uint, null, muellerHash64),
                 // SSOHashMap!(uint, uint, null, wangMixHash64),
                 // SSOHashMap!(uint, uint, null, FNV!(64, true)),
                 OpenHashMap!(Nullable!(uint, uint.max), uint, hashOf),
                 OpenHashMap!(Nullable!(uint, uint.max), uint, FNV!(64, true)),

                 // ulong => ulong
                 // SSOHashMap!(ulong, ulong, null, muellerHash64),
                 // SSOHashMap!(ulong, ulong, null, wangMixHash64),
                 // SSOHashMap!(ulong, ulong, null, FNV!(64, true)),
                 OpenHashMap!(Nullable!(ulong, ulong.max), ulong, hashOf),
                 OpenHashMap!(Nullable!(ulong, ulong.max), ulong, FNV!(64, true)),
                 OpenHashMap!(Nullable!(ulong, ulong.max), ulong, wangMixHash64),

                 OpenHashMap!(Address, Address, hashOf),
                 OpenHashMap!(Address, Address, FNV!(64, true)),
                 OpenHashMap!(Address, Address, wangMixHash64),

                 // string => string
                 OpenHashMap!(string, string, hashOf),
                 OpenHashMap!(string, string, XXHash64),
                 OpenHashMap!(string, string, MurmurHash3!(128)),
                 OpenHashMap!(string, string, FNV!(64, true)),

                 // SSOString => SSOString
                 OpenHashMap!(SSOString, SSOString, hashOf),
                 OpenHashMap!(SSOString, SSOString, FNV!(64, true)),
                 ))
    {
        A a;

        writef("- ");

        // allocate
        const keys = iotaArrayOf!(A.KeyType)(0, elementCount);

        {
            auto spans_ns = DynamicArray!double.withLength(runCount);
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    a.insert(A.ElementType(keys[i], A.ValueType.init));
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef("insert (w growth): %3.1f ns/op",
                   minElement(spans_ns[]) / elementCount);
        }

        {
            bool okAll = true;
            auto spans_ns = DynamicArray!double.withLength(runCount);
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
            auto spans_ns = DynamicArray!double.withLength(runCount);
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
            auto spans_ns = DynamicArray!double.withLength(runCount);
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

    writefln("\nBuiltin Assocative Arrays:\n");

    foreach (E; AliasSeq!(uint, ulong, string))
    {
        alias KeyType = E;
        alias ValueType = E;
        alias A = ValueType[KeyType];
        A a = A.init;

        writef("- ");

        // allocate
        const es = iotaArrayOf!E(0, elementCount);

        // insert
        {
            auto spans_ns = DynamicArray!double.withLength(runCount);
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    a[es[i]] = ValueType.init;
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef("insert (w growth): %3.1f ns/op", minElement(spans_ns[]) / elementCount);
        }

        // in
        {
            auto spans_ns = DynamicArray!double.withLength(runCount);
            bool okAll = true;
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                size_t hitCount = 0;
                foreach (immutable i; testSource)
                {
                    hitCount += cast(bool)(es[i] in a);
                }
                const ok = hitCount == elementCount; // for side effect in output
                if (!ok) { okAll = false; }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", contains: %3.1f ns/op (%s)", minElement(spans_ns[]) / elementCount, okAll ? "OK" : "ERR");
        }

        // rahash
        {
            auto spans_ns = DynamicArray!double.withLength(runCount);
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                a.rehash();
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", rehash: %3.1f ns/op", maxElement(spans_ns[]) / elementCount);
        }

        // in
        {
            auto spans_ns = DynamicArray!double.withLength(runCount);
            foreach (const runIx; 0 .. runCount)
            {
                immutable startTime = MonoTime.currTime();
                foreach (immutable i; testSource)
                {
                    const hit = es[i] in a;
                }
                spans_ns[runIx] = cast(double)(MonoTime.currTime() - startTime).total!"nsecs";
            }
            writef(", contains (after rehash): %3.1f ns/op", minElement(spans_ns[]) / elementCount);
        }

        writef(` for %s`, A.stringof);

        writeln();

        static if (hasMember!(A, `clear`))
        {
            a.clear();
        }
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
