import core.stdc.stdio: printf;

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
    static foreach (const i; 0 .. 31)
    {
        {
            enum byteCount = 2^^i;
            const Duration[1] resultsC = benchmark!(mallocAndFreeBytes!(i))(benchmarkCount);
            printf("%d bytes: mallocAndFreeBytes: %f nsecs",
                   byteCount, cast(double)resultsC[0].total!"nsecs"/benchmarkCount);

            import core.memory : GC;
            auto dArray = new byte[byteCount]; // one Gig
            const Duration[1] resultsD = benchmark!(GC.collect)(benchmarkCount);
            printf("  GC.collect(): %f nsecs after %p\n",
                     cast(double)resultsD[0].total!"nsecs"/benchmarkCount, dArray.ptr);
            dArray = null;
        }
    }
}
