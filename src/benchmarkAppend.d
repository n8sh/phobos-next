version = benchmark;

version(benchmark)
void main()
{
    // standard storage
    import std.array : Appender;
    import std.container.array : StdArray = Array;

    // my containers
    import basic_copyable_array : CopyableArray;
    import variant_arrays : VariantArrays;
    import hashset : HashSet, identityHashOf, murmurHash3Of, xxhash64Of, typeidHashOf;
    // import trie : RadixTreeSetGrowOnly;

    import std.stdio : writeln;
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
        // a.reserve(n);
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            a ~= cast(E)i;      // need to cast away const here
        }
        immutable after = MonoTime.currTime();
        writeln("Added ", n, " integers into ", A.stringof, " in ", after - before);
    }

    foreach (A; AliasSeq!(HashSet!(E, null, identityHashOf),
                          HashSet!(E, null, typeidHashOf),
                          HashSet!(E, null, hashOf),
                          HashSet!(E, null, murmurHash3Of),
                          HashSet!(E, null, xxhash64Of),
                          ))
    {
        A a = A.withCapacity(n);
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            a.insert(i);
        }
        immutable after = MonoTime.currTime();
        writeln("Inserted ", n, " integers into ", A.stringof, " in ", after - before);
    }

    foreach (A; AliasSeq!(bool[E]))
    {
        A a = A.init;
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            a[i] = true;
        }
        immutable after = MonoTime.currTime();
        writeln("Inserted ", n, " integers into ", A.stringof, " in ", after - before);
    }
}
