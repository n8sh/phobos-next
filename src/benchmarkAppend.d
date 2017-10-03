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
    import hashset : HashSet;

    import std.digest.murmurhash : MurmurHash3;
    import xxhash64 : XXHash64;
    import hash_functions;
    import digestx.fnv : FNV;

    import filters : DenseSetFilter;
    import filterarray : DenseSetFilterGrowableArray;

    // import digestx.fnv : fnv64aOf;
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
        A a;

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

                          HashSet!(E, null, identityHashOf),
                          HashSet!(E, null, typeidHashOf),
                          HashSet!(E, null, hashOf),
                          HashSet!(E, null, muellerHash64),
                          HashSet!(E, null, MurmurHash3!(128)),
                          HashSet!(E, null, FNV!(64, true)),
                          HashSet!(E, null, XXHash64),

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
            write("Inserted ", n, " integers in ", after - before);
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                assert(a.contains(i));
            }
            immutable after = MonoTime.currTime();
            write(", Checked ", n, " integers in ", after - before);
        }

        writeln(` for `, A.stringof);

        a.clear();
    }

    foreach (A; AliasSeq!(bool[E]))
    {
        A a = A.init;

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                a[i] = true;
            }
            immutable after = MonoTime.currTime();
            write("Inserted ", n, " integers in ", after - before);
        }

        {
            immutable before = MonoTime.currTime();
            foreach (const i; 0 .. n)
            {
                assert(i in a);
            }
            immutable after = MonoTime.currTime();
            write(", Checked ", n, " integers in ", after - before);
        }

        writeln(` for `, A.stringof);
    }
}
