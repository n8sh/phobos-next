void main()
{
    import std.array : Appender;
    import std.container.array : StdArray = Array;
    import basic_copyable_array : CopyableArray;
    import variant_arrays : VariantArrays;
    import hashset : HashSet;

    import std.stdio : writeln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;

    alias E = uint;
    immutable n = 5_000_000;

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

    foreach (A; AliasSeq!(HashSet!E))
    {
        A a = A(n);
        immutable before = MonoTime.currTime();
        foreach (const i; 0 .. n)
        {
            a.insert(i);
        }
        immutable after = MonoTime.currTime();
        writeln("Inserted ", n, " integers into ", A.stringof, " in ", after - before);
    }
}
