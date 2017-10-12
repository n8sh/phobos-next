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

    import std.conv : to;

    immutable n = 1_000_000;

    writeln("\nArrays:\n");

    foreach (A; AliasSeq!(CopyableArray!uint,
                          VariantArrays!uint,
                          StdArray!uint,
                          Appender!(uint[]),
                          uint[]))
    {
        write("- ");

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
        write("Appended ", n, " integers in ", (after - before).total!"msecs", " msecs");

        writeln(` for `, A.stringof);

        // static if (hasMember!(A, `clear`)) { a.clear(); }
    }

    writeln("\nSets:\n");

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
                          HashSet!(ulong, null, FNV!(64, true), 2),
                          HashSet!(ulong, null, FNV!(64, true), 3),
                          HashSet!(ulong, null, FNV!(64, true), 4),
                          HashSet!(uint, null, XXHash64),

                          // radix tree
                          RadixTreeSetGrowOnly!(uint),
                 ))
    {
        // scope
        A a;

        write("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a.insert(i);
            }
            immutable after = MonoTime.currTime();
            write("Insert (w growth): ", (after - before).total!"msecs", " msecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = a.contains(i);
            }
            immutable after = MonoTime.currTime();
            write(", Checking: ", (after - before).total!"msecs", " msecs");
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
            write(", Insertion (no growth): ", (after - before).total!"msecs", " msecs");
        }

        write(` for `, A.stringof);

        static if (hasMember!(A, `bucketCounts`))
        {
            write(" ", a.bucketCounts());
        }

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }

    writeln("\nMaps:\n");

    foreach (A; AliasSeq!(HashMap!(uint, uint, null, muellerHash64),
                          HashMap!(uint, uint, null, FNV!(64, true)),

                          HashMap!(ulong, ulong, null, muellerHash64),
                          HashMap!(ulong, ulong, null, FNV!(64, true)),
                          HashMap!(ulong, ulong, null, FNV!(64, true), 2)))
    {
        A a;

        write("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a.insert(A.ElementType(i, A.ValueType.init));
            }
            immutable after = MonoTime.currTime();
            write("Insert (w growth): ", (after - before).total!"msecs", " msecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = a.contains(A.ElementType(i, A.ValueType.init));
            }
            immutable after = MonoTime.currTime();
            write(", Checking: ", (after - before).total!"msecs", " msecs");
        }

        A b = A.withCapacity(n);
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            b.insert(A.ElementType(i, A.ValueType.init));
        }
        immutable after = MonoTime.currTime();
        write(", Insertion (no growth): ", (after - before).total!"msecs", " msecs");

        write(` for `, A.stringof);

        static if (hasMember!(A, `bucketCounts`))
        {
            write(" ", a.bucketCounts());
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

        write("- ");

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a[i] = ValueType.init;
            }
            immutable after = MonoTime.currTime();
            write("Insert (w growth): ", (after - before).total!"msecs", " msecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = i in a;
            }
            immutable after = MonoTime.currTime();
            write(", Checking: ", (after - before).total!"msecs", " msecs");
        }

        {
            immutable before = MonoTime.currTime();
            a.rehash();
            immutable after = MonoTime.currTime();
            write(", Rehashing: ", (after - before).total!"msecs", " msecs");
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                const hit = i in a;
            }
            immutable after = MonoTime.currTime();
            write(", Checking (after rehash): ", (after - before).total!"msecs", " msecs");
        }

        write(` for `, A.stringof);

        writeln();

        static if (hasMember!(A, `clear`)) { a.clear(); }
    }
}
