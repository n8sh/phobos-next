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
    import hashset : HashSet;

    import std.digest.murmurhash : MurmurHash3;
    import xxhash64 : XXHash64;
    import digestx.fnv : FNV;

    // import digestx.fnv : fnv64aOf;
    import trie : RadixTreeSetGrowOnly;

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
                          HashSet!(E, null, MurmurHash3!(128)),
                          HashSet!(E, null, FNV!(64, true)),
                          HashSet!(E, null, XXHash64),
                          RadixTreeSetGrowOnly!(E),
                          ))
    {
        import std.traits : hasMember;
        static if (hasMember!(A, `withCapacity`))
        {
            A a = A.withCapacity(n);
        }
        else
        {
            A a;
        }
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

import std.traits : isUnsigned;

/** Dummy-hash for benchmarking performance of HashSet. */
pragma(inline, true)
ulong identityHashOf(T)(in T value)
if (isUnsigned!T &&
        T.sizeof <= size_t.sizeof)
{
    return value;
}

/** See also: http://forum.dlang.org/post/o1igoc$21ma$1@digitalmars.com
    Doesn't work: integers are returned as is.
 */
pragma(inline, true)
size_t typeidHashOf(T)(in T value) @trusted
{
    return typeid(T).getHash(&value);
}
