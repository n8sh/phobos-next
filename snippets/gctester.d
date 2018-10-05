import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;
import core.time : Duration;
import std.datetime.stopwatch : benchmark;

void main(string[] args)
{
    struct Vec2d { double x, y; }
    benchmarkAllocate!Vec2d();
    benchmarkCollect();
}

size_t benchmarkAllocate(T)() @trusted
{
    immutable benchmarkCount = 1000;
    immutable iterationCount = 100;

    static immutable value = "123456789_123456";
    size_t ptrSum;

    void testNewAllocation() @safe pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = new T();
            ptrSum ^= cast(size_t)x; // for side effects
        }
    }

    // GC.disable();
    const Duration[1] results = benchmark!(testNewAllocation)(benchmarkCount);
    // GC.enable();

    writefln("- new a() %s took %s ns", T.stringof,
             cast(double)results[0].total!"nsecs"/(benchmarkCount*iterationCount));

    return ptrSum;              // side-effect
}

void benchmarkCollect() @safe
{
    immutable benchmarkCount = 10_000;

    void test() @trusted
    {
        GC.enable();
        GC.disable();
    }

    const Duration[1] results = benchmark!(test)(benchmarkCount);

    writefln("- enable()-disable() took %s ns",
             cast(double)results[0].total!"nsecs"/(benchmarkCount));
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
