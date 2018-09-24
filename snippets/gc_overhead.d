import std.stdio;

void main(string[] args)
{
    import std.datetime.stopwatch : benchmark;
    import core.time : Duration;
    import core.memory : GC;

    immutable benchmarkCount = 100;

    foreach (const i; 0 .. 10)
    {
        const array = new char[i*100_000_000]; // one Gig
        const Duration[1] results = benchmark!(GC.collect)(benchmarkCount);
        writefln("Calling GC.collect() took %s nsecs after %s",
                 cast(double)results[0].total!"nsecs"/benchmarkCount, array.ptr);
    }
}
