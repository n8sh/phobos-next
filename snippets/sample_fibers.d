module sample_fibers;

import core.thread: Fiber;

void fibonacciSeriesRef(ref int current)
{
    current = 0; // Note: 'current' is the parameter
    int next = 1;
    while (true)
    {
        Fiber.yield();
/* Next call() will continue from this point */
        const nextNext = current + next;
        current = next;
        next = nextNext;
    }
}

unittest
{
    int current;
    Fiber fiber = new Fiber(() => fibonacciSeriesRef(current));
    import std.stdio;
    foreach (_; 0 .. 10)
    {
        fiber.call(); // return
        write(current, ", ");
    }
    writeln;
}

import std.stdio;

import std.concurrency: yield;

void fibonacciSeries()
{
    int current = 0; // <-- Not a parameter anymore
    int next = 1;
    while (true)
    {
        current.yield; // return
        const nextNext = current + next;
        current = next;
        next = nextNext;
    }
}

unittest
{
    import std.concurrency: yield, Generator;
    auto series = new Generator!int(&fibonacciSeries);
    import std.range: take;
    writefln("%(%s, %)", series.take(10));
}

struct Yields { string type; }

@Yields("int")  // <-- better to specify type here than in caller
void fibonacciSeries2()
{
    int current = 0;
    int next = 1;
    while (true)
    {
        current.yield; // return
        const nextNext = current + next;
        current = next;
        next = nextNext;
    }
}

auto generator(alias func)()
{
    import std.format: format;
    foreach (attr; __traits(getAttributes, func))
    {
        import std.traits: isInstanceOf;
        static if (is (typeof(attr) == Yields))
        {
            mixin (format("alias YieldedType = %s;", attr.type));
            import std.concurrency: Generator;
            return new Generator!YieldedType(&func);
        }
    }
    assert(false, format("%s does not have a Yields attribute",
                         func.stringof));
}

unittest
{
    import std.stdio;
    import std.range: take;
    auto series = generator!fibonacciSeries2; // <-- THIS TIME, NO 'int'
    writefln("%(%s, %)", series.take(10));
}
