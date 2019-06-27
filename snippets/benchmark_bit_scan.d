import std.traits, std.meta, std.range, std.algorithm, std.stdio;
import std.datetime.stopwatch : benchmark;

@safe:

/**
 * Benchmark time it takes to scan n bits of data.
 */
void main(string[] args)
{
    enum totalByteCount = 16*1024*1024*1024; // total amount of RAM [bytes]
    enum allocByteCount = 16;                // allocation size [bytes]
    enum staticBitCount =  totalByteCount/allocByteCount;

    const n = 2*1024*1024;
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

    size_t[] x = new size_t[n]; // 2M * 8 bytes = 16 Megabytes
    size_t hit;
    void f() { hit = indexOfFirstBit(x); }

    writeln("duration: ", benchmark!(f)(1)[0]);
    writeln("duration: ", benchmark!(f)(1)[0]);
    writeln("duration: ", benchmark!(f)(1)[0]);
}
