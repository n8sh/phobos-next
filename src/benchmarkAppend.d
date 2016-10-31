void main()
{
    import std.container.array : CArray = Array;
    import std.array : Appender;
    import std.stdio : writeln;
    import std.datetime : StopWatch;
    import std.meta : AliasSeq;
    import array_ex : Array;
    import std.stdio : writeln;

    alias E = uint;
    const n = 5_000_000;

    foreach (A; AliasSeq!(Array!E,
                          E[],
                          Appender!(E[]),
                          CArray!E))
    {
        A a;

        StopWatch watch;
        watch.start;

        foreach (uint i; 0 .. n)
        {
            a ~= i;
        }

        watch.stop;
        writeln("Added ", n, " integer nodes into ", A.stringof, " in ", watch.peek.msecs, " ms.");
    }
}
