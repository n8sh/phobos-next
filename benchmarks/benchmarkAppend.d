void main()
{
    import std.container.array : StdArray = Array;
    import std.array : Appender;
    import std.stdio : writeln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;
    import std.algorithm.comparison : equal;
    import std.range : iota;
    import array_ex : Array;
    import std.stdio : writeln;

    alias E = uint;
    immutable n = 5_000_000;

    foreach (A; AliasSeq!(Array!E,
                          E[],
                          Appender!(E[]),
                          StdArray!E))
    {
        A a;

        immutable before = MonoTime.currTime();

        foreach (uint i; 0 .. n)
        {
            a ~= i;
        }

        immutable after = MonoTime.currTime();

        writeln("Added ", n, " integer nodes into ", A.stringof, " in ", after - before);
    }
}
