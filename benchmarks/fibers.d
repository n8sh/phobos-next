unittest
{
    import core.thread : Fiber;
    import std.stdio;

    class DerivedFiber : Fiber
    {
        this(size_t counter)
        {
            this.counter = counter;
            super(&run);
        }
    private :
        void run()
        {
        }
        size_t counter;
    }

    // void fiberFunc(size_t counter)
    // {
    //     Fiber.yield();
    // }

    immutable n = 100_000;
    foreach (immutable i; 0 .. n)
    {
        // create instances of each type
        Fiber derived = new DerivedFiber(0);
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
