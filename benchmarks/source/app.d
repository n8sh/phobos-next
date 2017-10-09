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

    import std.stdio : write, writeln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;

    alias E = uint;
    immutable n = 1_000_000;

    foreach (A; AliasSeq!(CopyableArray!E,
                          VariantArrays!E,
                          StdArray!E,
                          Appender!(E[]),
                          E[]))
    {
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
            a ~= cast(E)i;      // need to cast away const here
        }
        immutable after = MonoTime.currTime();
        write("Appended ", n, " integers in ", after - before);

        writeln(` for `, A.stringof);

        static if (hasMember!(A, `clear`))
        {
            a.clear();
        }
    }

    foreach (A; AliasSeq!(DenseSetFilter!(E),
                          DenseSetFilterGrowableArray!(E),

                          // functions
                          HashSet!(E, null, identityHash64Of),
                          HashSet!(E, null, typeidHashOf),
                          HashSet!(E, null, hashOf),
                          HashSet!(E, null, muellerHash64),
                          HashSet!(E, null, wangMixHash64),

                          // std.digests
                          HashSet!(E, null, MurmurHash3!(128)),
                          HashSet!(E, null, FNV!(64, true)),
                          HashSet!(E, null, XXHash64),

                          // radix tree
                          RadixTreeSetGrowOnly!(E),
                 ))
    {
        static if (hasMember!(A, `withCapacity`))
        {
            A a = A.withCapacity(n);
        }
        else
        {
            A a;
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a.insert(i);
            }
            immutable after = MonoTime.currTime();
            write("Insertion: ", after - before);
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = a.contains(i);
            }
            immutable after = MonoTime.currTime();
            write(", Checking: ", after - before);
        }

        static if (hasMember!(A, `bucketCounts`))
        {
            write(" ", a.bucketCounts());
        }

        writeln(` for `, A.stringof);

        a.clear();
    }

    alias ValueType = uint;

    foreach (A; AliasSeq!(HashMap!(E, ValueType, null, FNV!(64, true))))
    {
        A a = A.withCapacity(n);

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a.insert(A.ElementType(i, ValueType.init));
            }
            immutable after = MonoTime.currTime();
            write("Insertion: ", after - before);
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = a.contains(A.ElementType(i, ValueType.init));
            }
            immutable after = MonoTime.currTime();
            write(", Checking: ", after - before);
        }

        static if (hasMember!(A, `bucketCounts`))
        {
            write(" ", a.bucketCounts());
        }

        writeln(` for `, A.stringof);

        a.clear();
    }

    foreach (A; AliasSeq!(ValueType[E]))
    {
        A a = A.init;

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a[i] = ValueType.init;
            }
            immutable after = MonoTime.currTime();
            write("Insertion: ", after - before);
        }

        {
            immutable before = MonoTime.currTime();
            a.rehash();
            immutable after = MonoTime.currTime();
            write(", Rehashing: ", after - before);
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                assert(i in a);
            }
            immutable after = MonoTime.currTime();
            write(", Checking: ", after - before);
        }

        writeln(` for `, A.stringof);
    }
}
