version = benchmark;

version(benchmark)
void main()
{
    // standard storage
    import std.traits : hasMember;
    import std.array : Appender;
    import std.container.array : StdArray = Array;

    // my containers
    import basic_copyable_array : CopyableArray;
    import variant_arrays : VariantArrays;
    import hashmap : HashSet, HashMap;

    import std.digest.murmurhash : MurmurHash3;
    import xxhash64 : XXHash64;
    import hash_functions;
    import digestx.fnv : FNV;

    import filters : DenseSetFilter;
    import filterarray : DenseSetFilterGrowableArray;

    import trie : RadixTreeSetGrowOnly;

    import std.stdio : write, writeln, writef, writefln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;

    import std.conv : to;

    immutable n = 1024*1024;

    writefln("\nElement count: %s", n);

    writefln("\nArrays:\n");

    foreach (A; AliasSeq!(CopyableArray!uint,
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
            a.reserve(n);
        }
        else
        {
            A a;
        }

        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            a ~= i.to!uint;      // need to cast away const here
        }
        immutable after = MonoTime.currTime();
        writef("Appended: %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);

        writefln(` for %s`, A.stringof);

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }

    writefln("\nSets:\n");

    foreach (A; AliasSeq!(DenseSetFilter!(uint),
                          DenseSetFilterGrowableArray!(uint),

                          // functions
                          HashSet!(uint, null, identityHash64Of),
                          HashSet!(uint, null, typeidHashOf),
                          HashSet!(uint, null, hashOf),

                          HashSet!(uint, null, muellerHash64),
                          HashSet!(uint, null, wangMixHash64),
                          HashSet!(uint, null, FNV!(64, true)),

                          HashSet!(ulong, null, wangMixHash64),
                          HashSet!(ulong, null, muellerHash64),
                          HashSet!(ulong, null, FNV!(64, true), 2),
                          HashSet!(ulong, null, FNV!(64, true), 3),
                          HashSet!(ulong, null, FNV!(64, true), 4),

                          // std.digests
                          HashSet!(uint, null, MurmurHash3!(128)),
                          HashSet!(uint, null, XXHash64),

                          // radix tree
                          RadixTreeSetGrowOnly!(uint),
                 ))
    {
        // scope
        A a;

        writef("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a.insert(i);
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (const i; 0 .. n)
            {
                hitCount += a.contains(i);
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
            foreach (const i; 0 .. n)
            {
                b.insert(i);
            }
            immutable after = MonoTime.currTime();
            writef(", insert (no growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        writef(` for %s`, A.stringof);

        static if (hasMember!(A, `bucketCounts`))
        {
            writef(" %s", a.bucketCounts());
        }
        static if (hasMember!(A, `smallBucketCapacity`))
        {
            writef(" smallBucketCapacity:%s", A.smallBucketCapacity);
        }

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }

    writefln("\nMaps:\n");

    foreach (A; AliasSeq!(HashMap!(uint, uint, null, muellerHash64),
                          HashMap!(uint, uint, null, wangMixHash64),
                          HashMap!(uint, uint, null, FNV!(64, true)),

                          HashMap!(ulong, ulong, null, muellerHash64),
                          HashMap!(ulong, ulong, null, wangMixHash64),
                          HashMap!(ulong, ulong, null, FNV!(64, true))))
    {
        A a;

        writef("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a.insert(A.ElementType(i, A.ValueType.init));
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (const i; 0 .. n)
            {
                hitCount += a.contains(A.ElementType(i, A.ValueType.init));
            }
            const ok = hitCount = n; // for side effect in output
            assert(ok);
            immutable after = MonoTime.currTime();
            writef(", contains: %3.1f ns/op (%s)", cast(double)(after - before).total!"nsecs" / n, ok ? "OK" : "ERR");
        }

        A b = A.withCapacity(n);
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            b.insert(A.ElementType(i, A.ValueType.init));
        }
        immutable after = MonoTime.currTime();
        writef(", insert (no growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);

        writef(` for %s`, A.stringof);

        static if (hasMember!(A, `bucketCounts`))
        {
            writef(" %s", a.bucketCounts());
        }
        static if (hasMember!(A, `smallBucketCapacity`))
        {
            writef(" smallBucketCapacity:%s", A.smallBucketCapacity);
        }

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }

    foreach (E; AliasSeq!(uint, ulong))
    {
        alias KeyType = E;
        alias ValueType = E;
        alias A = ValueType[KeyType];
        A a = A.init;

        writef("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a[i] = ValueType.init;
            }
            immutable after = MonoTime.currTime();
            writef("insert (w growth): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        {
            immutable before = MonoTime.currTime();
            size_t hitCount = 0;
            foreach (const i; 0 .. n)
            {
                hitCount += cast(bool)(i in a);
            }
            const ok = hitCount = n; // for side effect in output
            assert(ok);
            immutable after = MonoTime.currTime();
            writef(", contains: %3.1f ns/op (%s)", cast(double)(after - before).total!"nsecs" / n, ok ? "OK" : "ERR");
        }

        {
            immutable before = MonoTime.currTime();
            a.rehash();
            immutable after = MonoTime.currTime();
            writef(", rehash: %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = i in a;
            }
            immutable after = MonoTime.currTime();
            writef(", contains (after rehash): %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
        }

        writef(` for %s`, A.stringof);

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }
}
