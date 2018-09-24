import std.stdio;

void main(string[] args)
{
    import std.datetime.stopwatch : benchmark;
    import core.time : Duration;

    immutable benchmarkCount = 1;

    // GC
    foreach (const i; 0 .. 10)
    {
        import core.memory : GC;
        const byteCount = i*100_000_000;
        const array = new byte[byteCount]; // one Gig
        const Duration[1] results = benchmark!(GC.collect)(benchmarkCount);
        writefln("%s bytes: Calling GC.collect() took %s nsecs after %s",
                 byteCount, cast(double)results[0].total!"nsecs"/benchmarkCount, array.ptr);
    }
}
