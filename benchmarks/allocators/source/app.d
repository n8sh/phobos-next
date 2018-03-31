import std.datetime.stopwatch : benchmark;
import std.stdio;
import std.experimental.allocator;
import std.experimental.allocator.building_blocks;

void benchmarkAllocatorsFreeList()
{
    alias UnboundedFreeList = FreeList!(GCAllocator, 0, unbounded);
    alias A = Segregator!(
        8, FreeList!(GCAllocator, 1, 8),
        16, FreeList!(GCAllocator, 9, 16),
        // 128, Bucketizer!(UnboundedFreeList, 1, 128, 16),
        // 256, Bucketizer!(UnboundedFreeList, 129, 256, 32),
        // 512, Bucketizer!(UnboundedFreeList, 257, 512, 64),
        // 1024, Bucketizer!(UnboundedFreeList, 513, 1024, 128),
        // 2048, Bucketizer!(UnboundedFreeList, 1025, 2048, 256),
        // 3584, Bucketizer!(UnboundedFreeList, 2049, 3584, 512),
        GCAllocator
        );
    A a;

    immutable n = 10_000_000;
    immutable wordCount = 4;

    /* store latest pointer here to prevent scoped allocation in clever
     * compilers such as LDC */
    void* latestPtr;

    void testNew()
    {
        auto x = new size_t[wordCount];
        latestPtr = x.ptr;
    }

    void testAllocator()
    {
        auto x = a.allocate(size_t.sizeof*wordCount);
        latestPtr = x.ptr;
    }

    const results = benchmark!(testNew, testAllocator)(n);
    writeln("new-allocation: ", results[0]);
    writeln("stdx-allocation: ", results[1]);
}

void main()
{
    benchmarkAllocatorsFreeList();
}
