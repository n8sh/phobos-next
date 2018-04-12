void main()
{
    // standard storage
    import std.traits : hasMember;
    import std.array : Appender;
    import std.container.array : StdArray = Array;
    import std.container.rbtree : RedBlackTree;

    // my containers
    import basic_array : BasicArray;
    import array_help : toUbytes;
    import variant_arrays : VariantArrays;
    import sso_hashmap_or_hashset : SSOHashSet, SSOHashMap;
    import open_hashmap_or_hashset : OpenHashMap, OpenHashSet;

    import std.digest.murmurhash : MurmurHash3;
    import xxhash64 : XXHash64;
    import hash_functions;
    import digestx.fnv : FNV;

    import filters : DenseSetFilter;
    import filterarray : DenseSetFilterGrowableArray;

    import std.typecons : Nullable;
    import trie : RadixTreeSetGrowOnly;

    import std.stdio : write, writeln, writef, writefln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;

    import std.conv : to;

    immutable n = 1024*1024;

    writefln("\nElement count: %s", n);

    writefln("\nArrays:\n");

    foreach (A; AliasSeq!(BasicArray!uint,
                          VariantArrays!uint,
                          StdArray!uint,
                          Appender!(uint[]),
                          uint[]))
    {
        writef("- ");

        static if (hasMember!(A, `withCapacity`))
        {
            auto a = A.withCapacity(n);
        }
        else static if (hasMember!(A, `reserve`))
        {
            A a;
            static if (hasMember!(A, `reserve`) &&
                       __traits(compiles, { a.reserve(n); }))
            {
                a.reserve(n);
            }
            else static if (hasMember!(A, `reserve`) &&
                            __traits(compiles, { a.reserve!uint(n); }))
            {
                a.reserve!uint(n);
            }
        }
        else
        {
            A a;
        }

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            a ~= i.to!uint;     // need to cast away const here for now. TODO remove this requirement
        }
        immutable after = MonoTime.currTime();
        writef("Appended: %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);

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
                          SSOHashSet!(uint, null, identityHash64Of),
                          SSOHashSet!(uint, null, typeidHashOf),
                          SSOHashSet!(uint, null, hashOf),

                          SSOHashSet!(uint, null, muellerHash64),
                          SSOHashSet!(uint, null, wangMixHash64),
                          SSOHashSet!(uint, null, FNV!(64, true)),

                          // std.digests
                          SSOHashSet!(uint, null, MurmurHash3!(128)),
                          SSOHashSet!(uint, null, XXHash64),

                          OpenHashSet!(Nullable!(uint, uint.max), FNV!(64, true)),

                          RadixTreeSetGrowOnly!(uint),
                          RedBlackTree!(uint),

                          SSOHashSet!(ulong, null, wangMixHash64),
                          SSOHashSet!(ulong, null, muellerHash64),
                          SSOHashSet!(ulong, null, FNV!(64, true), 2),
                          SSOHashSet!(ulong, null, FNV!(64, true), 3),
                          SSOHashSet!(ulong, null, FNV!(64, true), 4),

                          OpenHashSet!(Nullable!(ulong, ulong.max), FNV!(64, true)),

                          RadixTreeSetGrowOnly!(ulong),
                          RedBlackTree!(ulong),

                          // TODO OpenHashMap!(string, void, FNV!(64, true)),
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

        // TODO const elements = iotaArrayOf!(A.ElementType)(n);

        writef("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (immutable i; 0 .. n)
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
                        const element = A.ElementType(i); // wrap in i in Nullable
                    }
                    else
                    {
                        const element = i;
                    }
                    a.insert(element);
                }
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (immutable i; 0 .. n)
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
                        const element = A.ElementType(i); // wrap in i in Nullable
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
            const ok = hitCount = n; // for side effect in output
            assert(ok);
            immutable after = MonoTime.currTime();
            writef(", contains: %3.1f ns/op (%s)", cast(double)(after - before).total!"nsecs" / n, ok ? "OK" : "ERR");
        }

        static if (hasMember!(A, `withCapacity`))
        {
            A b = A.withCapacity(n);

            immutable before = MonoTime.currTime();
            foreach (immutable i; 0 .. n)
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
                        const element = A.ElementType(i); // wrap in i in Nullable
                    }
                    else
                    {
                        const element = i;
                    }
                    b.insert(element);
                }
            }
            immutable after = MonoTime.currTime();
            writef(", insert (no growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
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

        static if (hasMember!(A, `clear`))
        {
            a.clear();
        }
    }

    writefln("\nMaps:\n");

    foreach (A; AliasSeq!(SSOHashMap!(uint, uint, null, muellerHash64),
                          SSOHashMap!(uint, uint, null, wangMixHash64),
                          SSOHashMap!(uint, uint, null, FNV!(64, true)),
                          OpenHashMap!(Nullable!(uint, uint.max), uint, FNV!(64, true)),

                          SSOHashMap!(ulong, ulong, null, muellerHash64),
                          SSOHashMap!(ulong, ulong, null, wangMixHash64),
                          SSOHashMap!(ulong, ulong, null, FNV!(64, true)),
                          OpenHashMap!(Nullable!(ulong, ulong.max), ulong, FNV!(64, true)),

                          OpenHashMap!(string, string, FNV!(64, true)),
                 ))
    {
        A a;

        writef("- ");

        // allocate
        const keys = iotaArrayOf!(A.KeyType)(n);

        {
            immutable before = MonoTime.currTime();
            foreach (immutable i; 0 .. n)
            {
                a.insert(A.ElementType(keys[i], A.ValueType.init));
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (immutable i; 0 .. n)
            {
                hitCount += a.contains(keys[i]);
            }
            const ok = hitCount = n; // for side effect in output
            assert(ok);
            immutable after = MonoTime.currTime();
            writef(", contains: %3.1f ns/op (%s)", cast(double)(after - before).total!"nsecs" / n, ok ? "OK" : "ERR");
        }

        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (immutable i; 0 .. n)
            {
                hitCount += cast(bool)(keys[i] in a);
            }
            const ok = hitCount = n; // for side effect in output
            assert(ok);
            immutable after = MonoTime.currTime();
            writef(", in: %3.1f ns/op (%s)", cast(double)(after - before).total!"nsecs" / n, ok ? "OK" : "ERR");
        }

        A b = A.withCapacity(n);
        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            b.insert(A.ElementType(keys[i], A.ValueType.init));
        }
        immutable after = MonoTime.currTime();
        writef(", insert (no growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);

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
        const es = iotaArrayOf!E(n);

        // insert
        {
            immutable before = MonoTime.currTime();
            foreach (immutable i; 0 .. n)
            {
                a[es[i]] = ValueType.init;
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        // in
        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (immutable i; 0 .. n)
            {
                hitCount += cast(bool)(es[i] in a);
            }
            const ok = hitCount = n; // for side effect in output
            assert(ok);
            immutable after = MonoTime.currTime();
            writef(", contains: %3.1f ns/op (%s)", cast(double)(after - before).total!"nsecs" / n, ok ? "OK" : "ERR");
        }

        // rahash
        {
            immutable before = MonoTime.currTime();
            a.rehash();
            immutable after = MonoTime.currTime();
            writef(", rehash: %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        // in
        {
            immutable before = MonoTime.currTime();
            foreach (immutable i; 0 .. n)
            {
                const hit = es[i] in a;
            }
            immutable after = MonoTime.currTime();
            writef(", contains (after rehash): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        writef(` for %s`, A.stringof);

        writeln();

        static if (hasMember!(A, `clear`))
        {
            a.clear();
        }
    }
}

T[] iotaArrayOf(T, U)(U n)
{
    typeof(return) es = new T[n];
    foreach (immutable i; 0 .. n)
    {
        import std.conv : to;
        static if (is(typeof(T(i)))) // if possible
        {
            es[i] = T(i);       // try normal construction
        }
        else
        {
            es[i] = i.to!T;     // otherwise conv which may allocate
        }
    }
    return es;
}
