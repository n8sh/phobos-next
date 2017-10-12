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

    immutable n = 512*1024 + 1;

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
        writef("Appended: %6s us", (after - before).total!"usecs");

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

                          // std.digests
                          HashSet!(uint, null, MurmurHash3!(128)),
                          HashSet!(uint, null, FNV!(64, true)),
                          HashSet!(uint, null, XXHash64),

                          HashSet!(ulong, null, muellerHash64),
                          HashSet!(ulong, null, FNV!(64, true), 2),
                          HashSet!(ulong, null, FNV!(64, true), 3),
                          HashSet!(ulong, null, FNV!(64, true), 4),

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
            writef("Insert (w growth): %6s us", (after - before).total!"usecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = a.contains(i);
            }
            immutable after = MonoTime.currTime();
            writef(", Checking: %6s us", (after - before).total!"usecs");
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
            writef(", Insert (no growth): %6s us", (after - before).total!"usecs");
        }

        writef(` for %s`, A.stringof);

        static if (hasMember!(A, `bucketCounts`))
        {
            writef(" %s", a.bucketCounts());
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
                          HashMap!(ulong, ulong, null, FNV!(64, true)),
                          HashMap!(ulong, ulong, null, FNV!(64, true), 2)))
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
            writef("Insert (w growth): %6s us", (after - before).total!"usecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = a.contains(A.ElementType(i, A.ValueType.init));
            }
            immutable after = MonoTime.currTime();
            writef(", Checking: %6s us", (after - before).total!"usecs");
        }

        A b = A.withCapacity(n);
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            b.insert(A.ElementType(i, A.ValueType.init));
        }
        immutable after = MonoTime.currTime();
        writef(", Insert (no growth): %6s us", (after - before).total!"usecs");

        writef(` for %s`, A.stringof);

        static if (hasMember!(A, `bucketCounts`))
        {
            writef(" %s", a.bucketCounts());
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
            writef("Insert (w growth): %6s us", (after - before).total!"usecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = i in a;
            }
            immutable after = MonoTime.currTime();
            writef(", Checking: %6s us", (after - before).total!"usecs");
        }

        {
            immutable before = MonoTime.currTime();
            a.rehash();
            immutable after = MonoTime.currTime();
            writef(", Rehashing: %6s us", (after - before).total!"usecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = i in a;
            }
            immutable after = MonoTime.currTime();
            writef(", Checking (after rehash): %6s us", (after - before).total!"usecs");
        }

        writef(` for %s`, A.stringof);

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }
}
