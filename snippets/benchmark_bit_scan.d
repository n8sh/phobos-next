import std.traits, std.meta, std.range, std.algorithm, std.stdio;
import std.datetime.stopwatch : StopWatch, AutoStart, benchmark;

@safe:

/**
 * Benchmark time it takes to scan status bits for typical GC allocations of typical size.
 */
void main(string[] args)
{
    enum totalByteCount = 16*1024*1024*1024UL; // total amount of RAM [bytes]
    enum allocByteCount = 16;                  // allocation size [bytes]
    enum statusBitCount =  totalByteCount/allocByteCount; // number of status bits
    writeln("Status bit count: ", statusBitCount);
    enum wordBitCount = 64;     // bit per word (`size_t`)
    enum statusWordCount = statusBitCount/wordBitCount;
    writeln("Status word count: ", statusWordCount);
    writeln("Status bits size: ", statusWordCount*8);

    size_t indexOfFirstBit(const scope size_t[] x) @safe pure nothrow @nogc
    {
        return x.find(1).length;
        // typeof(return) sum = 0;
        // foreach (const ix, const ref e; x)
        // {
        //     if (e != 0)
        //     {
        //         return ix;
        //     }
        // }
        // return x.length;
    }

    size_t[] x = new size_t[statusWordCount];
    size_t index = 0;

    size_t f() @safe pure nothrow @nogc
    {
        x[statusWordCount/2 - (index++)] = 1; // set half-way for average case search performance
        return indexOfFirstBit(x);
    }

    const uint benchmarkCount = 1;

    auto sw = StopWatch(AutoStart.yes);

    foreach (const i; 0 .. 10)
    {
        sw.reset();
        const hit = f();
        writeln("Took: ", sw.peek(),
                " hit: ", hit);
    }
}
