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
    static foreach (const size_t i; 0 .. 32)
    {
        {
            enum byteCount = 2UL^^i;
            const Duration[1] resultsC = benchmark!(mallocAndFreeBytes!(i))(benchmarkCount);
            printf("%ld bytes: mallocAndFreeBytes: %ld nsecs",
                   byteCount, cast(size_t)(cast(double)resultsC[0].total!"nsecs"/benchmarkCount));

            import core.memory : GC;
            auto dArray = new byte[byteCount]; // one Gig
            const Duration[1] resultsD = benchmark!(GC.collect)(benchmarkCount);
            printf("  GC.collect(): %ld nsecs after %p\n",
                   cast(size_t)(cast(double)resultsD[0].total!"nsecs"/benchmarkCount), dArray.ptr);
            dArray = null;
        }
    }
}
