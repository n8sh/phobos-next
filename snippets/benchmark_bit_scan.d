import std.traits, std.meta, std.range, std.algorithm, std.stdio;
import std.datetime.stopwatch : benchmark;

@safe:

/**
 * Benchmark time it takes to scan status bits for typical GC allocations of typical size.
 */
void main(string[] args)
{
    enum totalByteCount = 16*1024*1024*1024UL; // total amount of RAM [bytes]
    enum allocByteCount = 16;                // allocation size [bytes]
    enum statusBitCount =  totalByteCount/allocByteCount; // number of status bits
    enum wordBitCount = 64;                               // bit per word (`size_t`)
    enum statusWordCount = statusBitCount/wordBitCount;
    pragma(msg, statusWordCount);
    size_t indexOfFirstBit(const scope size_t[] x) @safe pure nothrow @nogc
    {
        typeof(return) sum = 0;
        foreach (const ix, const ref e; x)
        {
            if (e != 0)
            {
                return ix;
            }
        }
        return x.length;
    }

    size_t[] x = new size_t[statusWordCount];
    size_t hit;
    void f() { hit = indexOfFirstBit(x); }

    writeln("duration: ", benchmark!(f)(1)[0]);
    writeln("duration: ", benchmark!(f)(1)[0]);
    writeln("duration: ", benchmark!(f)(1)[0]);
}
