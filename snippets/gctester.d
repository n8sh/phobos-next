import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;
import core.time : Duration;
import std.datetime.stopwatch : benchmark;

void main(string[] args)
{
    struct Vec2d
    {
        double x, y;
    }
    benchmarkAllocate!Vec2d();
}

void benchmarkAllocate(T)() @trusted
{
    immutable benchmarkCount = 100_000;

    static immutable value = "123456789_123456";
    size_t ptrSum;

    void testNewAllocation() @safe pure
    {
        auto x = new T();
        ptrSum ^= cast(size_t)x; // for side effects
    }

    // GC.disable();
    const Duration[1] results = benchmark!(testNewAllocation)(benchmarkCount);
    // GC.enable();

    writefln("Allocating on element of type %s took %s ns ptrSum:%s",
             T.stringof,
             cast(double)results[0].total!"nsecs"/benchmarkCount,
             ptrSum);
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
