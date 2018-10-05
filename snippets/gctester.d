import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;

void main(string[] args)
{
    benchmarkAllocateStrings();
}

import std.datetime.stopwatch : benchmark;

void benchmarkAllocateStrings() @trusted
{
    immutable benchmarkCount = 100_000;

    static immutable value = "123456789_123456";
    immutable(char)* latestPtr;

    void testNewAllocation() @safe pure
    {
        auto x = value.idup;
        latestPtr = &x[0];
    }

    import core.time : Duration;
    import core.memory : GC;

    // GC.disable();
    const Duration[1] results = benchmark!(testNewAllocation)(benchmarkCount);
    // GC.enable();

    writefln("Allocating string took %s ns",
             cast(double)results[0].total!"nsecs"/benchmarkCount);
}

void simpleBenchmark()
{
    const n = 1024*1024;
    alias T = long;
    size_t xx = 0;
    foreach (i; 0 .. n)
    {
        T* x = new T(i);
        xx ^= cast(size_t)x;
        // printf("x: i:%d, p:%p\n", i, x);
        x = null;
        // GC.collect();
    }
}
