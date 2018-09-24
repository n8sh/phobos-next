import std.traits, std.meta, std.range, std.algorithm, std.stdio;

void main(string[] args)
{
    import std.datetime.stopwatch : benchmark;
    import core.time : Duration;
    import core.memory : GC;

    immutable benchmarkCount = 100_000;
    const Duration[1] results = benchmark!(GC.collect)(benchmarkCount);

    writefln("Calling GC.collect() took %s ns",
             cast(double)results[0].total!"nsecs"/benchmarkCount);
}
