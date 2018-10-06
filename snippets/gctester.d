import core.stdc.stdio: printf;
import core.memory : GC, pureMalloc, pureCalloc, pureFree;
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
    writeln("  ns/w: nanoseconds per word");
}

/** Benchmark a single `new`-allocation of `T` using GC.
 */
size_t benchmarkAllocation(E, uint n)() @trusted
{
    import std.traits : hasElaborateDestructor, hasIndirections;
    alias A = E[n];
    struct T { A a; }

    class X { A a; }
    static assert(!hasElaborateDestructor!X); // shouldn't need finalizer
    enum ba = (!hasIndirections!T) ? GC.BlkAttr.NO_SCAN : 0;

    size_t ptrSum;

    immutable benchmarkCount = 1000;
    immutable iterationCount = 100;

    void doNew() @safe pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = new T();
            ptrSum ^= cast(size_t)x; // side-effect
        }
    }

    void doGCMalloc() @trusted pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = GC.malloc(T.sizeof, ba);
            ptrSum ^= cast(size_t)x; // side-effect
        }
    }

    void doGCCalloc() @trusted pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = GC.calloc(T.sizeof, ba);
            ptrSum ^= cast(size_t)x; // side-effect
        }
    }

    void doMalloc() @trusted pure nothrow @nogc
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = pureMalloc(T.sizeof);
            ptrSum ^= cast(size_t)x; // side-effect
        }
    }

    void doCalloc() @trusted pure nothrow @nogc
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = pureCalloc(T.sizeof, 1);
            ptrSum ^= cast(size_t)x; // side-effect
        }
    }

    GC.disable();
    const results = benchmark!(doNew,
                               doGCMalloc,
                               doGCCalloc,
                               doMalloc,
                               doCalloc)(benchmarkCount);
    GC.enable();

    writef("-");

    writef(" T.sizeof:%4s bytes:  new:%4.1f ns/w  GC.malloc:%4.1f ns/w  GC.calloc:%4.1f ns/w  pureMalloc:%4.1f ns/w  pureCalloc:%4.1f ns/w",
           T.sizeof,
           cast(double)results[0].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[1].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[1].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[2].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[3].total!"nsecs"/(benchmarkCount*iterationCount*n));

    writeln();

    return ptrSum;              // side-effect
}

/** Benchmark a single call to enable and disable() using `GC`.
 */
void benchmarkEnableDisable() @safe
{
    immutable benchmarkCount = 1_000;

    void test() @trusted
    {
        GC.enable();
        GC.disable();
    }

    const Duration[1] results = benchmark!(test)(benchmarkCount);

    writefln("- enable()-disable(): %s ns",
             cast(double)results[0].total!"nsecs"/(benchmarkCount));
}
