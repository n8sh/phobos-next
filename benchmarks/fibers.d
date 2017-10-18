template isFiberParameter(T)
{
    import std.traits : hasAliasing;
    enum isFiberParameter = !hasAliasing!T;
}

unittest
{
    static assert(isFiberParameter!int);
    static assert(!isFiberParameter!(int*));
    static assert(isFiberParameter!string);
}

import std.traits : allSatisfy;
import core.thread : Fiber;
import std.stdio;

static immutable maxFiberCount = 100;

/** Function-like fiber.
 */
class FunFiber(Args...) : Fiber
    if (allSatisfy!(isFiberParameter, Args))
{
    /** Extend to wrapper that takes. */
    this(Args args)
    {
        _args = args;
        super(&run);
    }
private :
    void run()
    {
        writeln(_args);
    }
    Args _args;
}

alias TestFiber = FunFiber!size_t;

unittest
{
    foreach (immutable i; 0 .. maxFiberCount)
    {
        // create instances of each type
        auto derived = new TestFiber(i);
        // Fiber composed = new Fiber(&fiberFunc, i);

        // call both fibers once
        derived.call();
        // composed.call();
        // printf("Execution returned to calling context.\n");
        // composed.call();

        // since each fiber has run to completion, each should have state TERM
        assert(derived.state == Fiber.State.TERM);
        // assert(composed.state == Fiber.State.TERM);
    }
}
