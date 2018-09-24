import std.stdio;

void* mallocAndFreeBytes(size_t byteCount)()
{
    import core.memory : pureMalloc, pureFree;
    void* ptr = pureMalloc(byteCount);
    pureFree(ptr);
    return ptr;                 // for side-effects
}

void main(string[] args)
{
    import std.datetime.stopwatch : benchmark;
    import core.time : Duration;

    immutable benchmarkCount = 1;

    // GC
    static foreach (const i; 0 .. 10)
    {
        {
            enum byteCount = i*100_000_000;
            const Duration[1] resultsC = benchmark!(mallocAndFreeBytes!(i))(benchmarkCount);
            writefln("%s bytes: Calling mallocAndFreeBytes took %s nsecs",
                     byteCount, cast(double)resultsC[0].total!"nsecs"/benchmarkCount);

            import core.memory : GC;
            const dArray = new byte[byteCount]; // one Gig
            const Duration[1] resultsD = benchmark!(GC.collect)(benchmarkCount);
            writefln("%s bytes: Calling GC.collect() took %s nsecs after %s",
                     byteCount, cast(double)resultsD[0].total!"nsecs"/benchmarkCount, dArray.ptr);
        }
    }
}
