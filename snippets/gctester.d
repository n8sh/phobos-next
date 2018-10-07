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

extern (C) @safe pure nothrow
{
    static foreach (sizeClass; smallSizeClasses)
    {
        mixin("void* gc_malloc_" ~ sizeClass.stringof ~ "(uint ba = 0);");
    }
}

void main(string[] args)
{
    benchmarkEnableDisable();
    /* All but last, otherwise new C() fails below because it requires one extra
     * word for type-info. */
    static foreach (byteSize; smallSizeClasses[0 .. $ - 1])
    {
        {
            enum wordCount = byteSize/8;
            benchmarkAllocation!(ulong, wordCount)();
        }
    }
    writeln("  ns/w: nanoseconds per word");
}

static immutable benchmarkCount = 1000;
static immutable iterationCount = 100;

/** Benchmark a single `new`-allocation of `T` using GC.
 */
size_t benchmarkAllocation(E, uint n)() @trusted
{
    import std.traits : hasElaborateDestructor, hasIndirections;
    alias A = E[n];
    struct T { A a; }
    class C { A a; }
    static assert(!hasElaborateDestructor!C); // shouldn't need finalizer
    enum ba = (!hasIndirections!T) ? GC.BlkAttr.NO_SCAN : 0;

    size_t ptrSum;

    void doNewClass() @trusted pure nothrow // TODO this crashes
    {
        foreach (const i; 0 .. iterationCount)
        {
            auto x = new C();   // allocates: `__traits(classInstanceSize, C)` bytes
            ptrSum ^= cast(size_t)(cast(void*)x); // side-effect
        }
    }

    void doNewStruct() @trusted pure nothrow
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

    void doGCNMalloc() @trusted pure nothrow
    {
        foreach (const i; 0 .. iterationCount)
        {
            static if (T.sizeof == 8)
            {
                auto x = gc_malloc_8(ba);
                ptrSum ^= cast(size_t)x; // side-effect
            }
            else static if (T.sizeof == 16)
            {
                auto x = gc_malloc_16(ba);
                ptrSum ^= cast(size_t)x; // side-effect
            }
            else static if (T.sizeof == 32)
            {
                auto x = gc_malloc_32(ba);
                ptrSum ^= cast(size_t)x; // side-effect
            }
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
    const results = benchmark!(doNewClass,
                               doNewStruct,
                               doGCMalloc,
                               doGCNMalloc,
                               doGCCalloc,
                               doMalloc,
                               doCalloc)(benchmarkCount);
    GC.enable();

    writef("-");

    writef(" T.sizeof:%4s bytes:  new-class:%4.1f ns/w  new-struct:%4.1f ns/w  GC.malloc:%4.1f ns/w  gc_malloc_%4u:%4.1f ns/w  GC.calloc:%4.1f ns/w  pureMalloc:%4.1f ns/w  pureCalloc:%4.1f ns/w",
           T.sizeof,
           cast(double)results[0].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[1].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[2].total!"nsecs"/(benchmarkCount*iterationCount*n),
           T.sizeof,
           cast(double)results[3].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[4].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[5].total!"nsecs"/(benchmarkCount*iterationCount*n),
           cast(double)results[6].total!"nsecs"/(benchmarkCount*iterationCount*n)
        );

    writeln();

    return ptrSum;              // side-effect
}

/** Benchmark a single call to enable and disable() using `GC`.
 */
void benchmarkEnableDisable() @safe
{
    void doEnableDisable() @trusted
    {
        foreach (const i; 0 .. iterationCount)
        {
            GC.enable();
            GC.disable();
        }
    }

    const Duration[1] results = benchmark!(doEnableDisable)(benchmarkCount);

    writefln("- enable()-disable(): %s ns",
             cast(double)results[0].total!"nsecs"/(benchmarkCount*iterationCount));
}
