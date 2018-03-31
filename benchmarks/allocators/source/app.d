import std.datetime.stopwatch : benchmark;
import std.stdio;
import std.experimental.allocator;
import std.experimental.allocator.building_blocks;

enum wordSize = size_t.sizeof;

class NodeD
{
    this(const double value) { this.value = value; }
    double value;
}
static assert(__traits(classInstanceSize, NodeD) == 24);
@safe pure nothrow @nogc unittest
{
    assert(NodeD.classinfo.m_init.length == 24);
}

extern(C++) class NodeCxx
{ extern(D):
    this(const double value) { this.value = value; }
    double value;
}
static assert(__traits(classInstanceSize, NodeCxx) == 16);
@safe pure nothrow @nogc unittest
{
    assert(NodeCxx.classinfo.m_init.length == 16);
}

class Graph
{
    alias NodeAllocator = Region!(NullAllocator, 8);
    this()
    {
        _region = GCAllocator.instance.allocate(1024*1024);
        _allocator = NodeAllocator(cast(ubyte[])_region);

        auto sampeNode = make!NodeD(42);
    }

    Type make(Type, Args...)(Args args)
        if (is(Type == class))
    {
        return _allocator.make!Type(args);
    }

    NodeAllocator _allocator;   // allocator over `_region`
    void[] _region;             // raw data for allocator
}

void benchmarkAllocatorsRegion()
{
    Graph graph;

    immutable nodeCount = 1000_000; // number of `Nodes`s to allocate
    void[] buf = GCAllocator.instance.allocate(nodeCount * __traits(classInstanceSize, NodeD));
    auto allocator = Region!(NullAllocator, 8)(cast(ubyte[])buf);

    auto allocate(Type)(double value)
        // TODO pure
    {
        pragma(inline, true);
        return allocator.make!Type(value);
    }

    /* store latest pointer here to prevent scoped allocation in clever
     * compilers such as LDC */
    void* latestPtr;

    void testNew()
    {
        auto x = new NodeD(42);
        latestPtr = cast(void*)x;
    }

    void testGlobalDefaultAllocator()
    {
        auto x = theAllocator.make!NodeD(42);
        latestPtr = cast(void*)x;
    }

    void testAllocator()
    {
        auto x = allocate!NodeD(42);
        latestPtr = cast(void*)x;
    }

    const results = benchmark!(testNew,
                               testGlobalDefaultAllocator,
                               testAllocator)(nodeCount);
    writeln("NodeD new-allocation: ", results[0]);
    writeln("NodeD with global allocator: ", results[1]);
    writeln("NodeD region allocator: ", results[2]);
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
