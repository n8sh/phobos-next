import std.datetime.stopwatch : benchmark;
import std.stdio;
import std.experimental.allocator;
import std.experimental.allocator.building_blocks;

void benchmarkAllocatorsRegion()
{
    class Node
    {
        double value;
    }

    enum nodeSize = __traits(classInstanceSize, Node);
    enum classWordOverhead = 3; // class size overhead is 3 words
    enum wordSize = size_t.sizeof;

    static assert(nodeSize == (classWordOverhead + 1)*wordSize);

    immutable nodeCount = 1000_000;
    void[] buf = GCAllocator.instance.allocate(nodeCount * nodeSize);
    auto allocator = Region!(NullAllocator, 8)(cast(ubyte[])buf);

    /* store latest pointer here to prevent scoped allocation in clever
     * compilers such as LDC */
    void* latestPtr;

    void testNew()
    {
        auto x = new Node();
        latestPtr = cast(void*)x;
    }

    void testAllocator()
    {
        auto x = allocator.make!Node();
        latestPtr = cast(void*)x;
    }

    const results = benchmark!(testNew, testAllocator)(nodeCount);
    writeln("Node new-allocation: ", results[0]);
    writeln("Node stdx-allocation: ", results[1]);
}

void benchmarkAllocatorsFreeList()
{
    alias UnboundedFreeList = FreeList!(GCAllocator, 0, unbounded);
    alias Allocator = Segregator!(8, FreeList!(GCAllocator, 1, 8),
                                  16, FreeList!(GCAllocator, 9, 16),
                                  // 128, Bucketizer!(UnboundedFreeList, 1, 128, 16),
                                  // 256, Bucketizer!(UnboundedFreeList, 129, 256, 32),
                                  // 512, Bucketizer!(UnboundedFreeList, 257, 512, 64),
                                  // 1024, Bucketizer!(UnboundedFreeList, 513, 1024, 128),
                                  // 2048, Bucketizer!(UnboundedFreeList, 1025, 2048, 256),
                                  // 3584, Bucketizer!(UnboundedFreeList, 2049, 3584, 512),
                                  GCAllocator);
    Allocator allocator;

    immutable nodeCount = 1_000_000;
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
        auto x = allocator.allocate(size_t.sizeof*wordCount);
        latestPtr = x.ptr;
    }

    const results = benchmark!(testNew, testAllocator)(nodeCount);
    writeln("new-allocation: ", results[0]);
    writeln("stdx-allocation: ", results[1]);
}

void main()
{
    benchmarkAllocatorsRegion();
    benchmarkAllocatorsFreeList();
}
