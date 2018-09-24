import std.stdio;

void main(string[] args)
{
    import std.datetime.stopwatch : benchmark;
    import core.time : Duration;
    import core.memory : GC;

    immutable benchmarkCount = 100;

    const Duration[1] results = benchmark!(GC.collect)(benchmarkCount);
    writefln("Calling GC.collect() took %s nsecs",
             cast(double)results[0].total!"nsecs"/benchmarkCount);

    const array1 = new char[1_000_000_000]; // one Gig
    const Duration[1] results1 = benchmark!(GC.collect)(benchmarkCount);
    writefln("Calling GC.collect() took %s nsecs after %s",
             cast(double)results1[0].total!"nsecs"/benchmarkCount, array1.ptr);

    const array2 = new char[1_000_000_000]; // one Gig
    const Duration[1] results2 = benchmark!(GC.collect)(benchmarkCount);
    writefln("Calling GC.collect() took %s nsecs after %s",
             cast(double)results[0].total!"nsecs"/benchmarkCount, array2.ptr);
}
