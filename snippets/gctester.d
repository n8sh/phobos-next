import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;
import core.time : Duration;
import std.datetime.stopwatch : benchmark;

/// Small slot sizes classes (in bytes).
static immutable smallSizeClasses = [8,
                                     16, // TODO 16 + 8,
                                     32, // TODO 32 + 16,
                                     64, // TODO 64 + 32,
                                     128, // TODO 128 +64,
                                     256, // TODO 256 + 128,
                                     512, // TODO 512 + 256,
                                     1024, // TODO 1024 + 512,
                                     2048, // TODO 2048 + 1024,
    ];

void main(string[] args)
{
    benchmarkEnableDisable();
    static foreach (byteSize; smallSizeClasses)
    {
        {
            enum wordCount = byteSize/8;
            benchmarkAllocation!(ulong, wordCount)();
        }
    }
}

/** Benchmark a single `new`-allocation of `T` using GC.
 */
size_t benchmarkAllocation(E, uint n)() @trusted
{
    alias A = E[n];
    struct T { A a; }
    enum size = T.sizeof;

    size_t ptrSum;

    immutable benchmarkCount = 1000;
    immutable iterationCount = 100;

    void exerciseNew() @safe pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = new T();
            ptrSum ^= cast(size_t)x; // for side effects
        }
    }

    void exerciseMalloc() @trusted pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = GC.malloc(size);
            ptrSum ^= cast(size_t)x; // for side effects
        }
    }

    GC.disable();
    const Duration[1] newResults = benchmark!(exerciseNew)(benchmarkCount);
    const Duration[1] mallocResults = benchmark!(exerciseMalloc)(benchmarkCount);
    GC.enable();

    writef("-");

    writef(" size:%4s: new:%2.1f ns/w GC.malloc:%2.1f ns/w", T.sizeof,
           cast(double)newResults[0].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)mallocResults[0].total!"nsecs"/(benchmarkCount*iterationCount*n));

    writeln();

    return ptrSum;              // side-effect
}

/** Benchmark a single call to enable and disable() using `GC`.
 */
void benchmarkEnableDisable() @safe
{
    immutable benchmarkCount = 10_000;

    void test() @trusted
    {
        GC.enable();
        GC.disable();
    }

    const Duration[1] results = benchmark!(test)(benchmarkCount);

    writefln("- enable()-disable(): %s ns",
             cast(double)results[0].total!"nsecs"/(benchmarkCount));
}
