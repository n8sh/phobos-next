import std.datetime.stopwatch : benchmark;
import std.stdio;
import std.experimental.allocator;
import std.experimental.allocator.building_blocks : NullAllocator, FreeList, Segregator;

import std.experimental.allocator.building_blocks.ascending_page_allocator : AscendingPageAllocator;
import std.experimental.allocator.building_blocks.aligned_block_list : AlignedBlockList;
import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;

import region_allocator : Region; // using my own until @safe fixes has been merged
import pure_gc_allocator : PureGCAllocator; // using my own until @safe fixes has been merged

enum wordSize = size_t.sizeof;

/** A node in the graph. */
extern(C++) class Node
{
    extern(D) @safe:                // makes memory overhead 1 word instead of 2
    string asMaybeString() { return null; }
}

/** A node in the graph. */
extern(C++) class DoubleNode : Node
{
extern(D) @safe:                // makes memory overhead 1 word instead of 2
    public:
    this(const double value) { this.value = value; }
    double value;
    override string asMaybeString()
    {
        import std.conv : to;
        return value.to!string;
    }
}
static assert(__traits(classInstanceSize, DoubleNode) == (1 + 1)*wordSize); // 1-word-overhead
@safe pure nothrow @nogc unittest
{
    assert(DoubleNode.classinfo.m_init.length == 16);
}

/** Graph managing objects. */
class Graph
{
    alias NodeRegionAllocator = Region!(NullAllocator, Node.alignof);
    this()
    {
        _regionBlock = PureGCAllocator.instance.allocate(1024*1024);
        _allocator = NodeRegionAllocator(cast(ubyte[])_regionBlock);

        auto sampleNode = make!DoubleNode(42);
    }

    Type make(Type, Args...)(Args args)
    if (is(Type == class))
    {
        pragma(inline, true);
        return _allocator.make!Type(args);
    }

    NodeRegionAllocator _allocator;   // allocator over `_regionBlock`
    void[] _regionBlock;             // raw data for allocator
}

/** Benchmark `Region` vs `PureGCAllocator`. */
void benchmarkAllocatorsRegion()
{
    immutable nodeCount = 10_000_000; // number of `Nodes`s to allocate

    void[] buf = PureGCAllocator.instance.allocate(nodeCount * __traits(classInstanceSize, DoubleNode));
    auto allocator = Region!(NullAllocator, platformAlignment)(cast(ubyte[])buf);

    Type make(Type, Args...)(Args args) // TODO @safe pure
    {
        pragma(inline, true);
        return allocator.make!Type(args);
    }

    void[] allocate(size_t bytes) // TOTDO @trusted pure
    {
        return allocator.allocate(bytes);
    }

    /* latest pointer here to prevent fast scoped non-GC allocation in LDC */
    void* latestPtr;

    void testRegionAllocator()
    {
        version(LDC) pragma(inline, true);
        auto x = make!DoubleNode(42);
        assert(x);
        latestPtr = cast(void*)x;
    }

    void testNewAllocation()
    {
        version(LDC) pragma(inline, true);
        auto x = new DoubleNode(42);
        latestPtr = cast(void*)x;
    }

    void testGlobalAllocator()
    {
        version(LDC) pragma(inline, true);
        auto x = theAllocator.make!DoubleNode(42);
        latestPtr = cast(void*)x;
    }

    const results = benchmark!(testRegionAllocator,
                               testNewAllocation,
                               testGlobalAllocator)(nodeCount);
    writeln("DoubleNode Region allocator: ", results[0]);
    writeln("DoubleNode new-allocation: ", results[1]);
    writeln("DoubleNode with global allocator: ", results[2]);

    PureGCAllocator.instance.deallocate(buf);
}

void benchmarkAllocatorsFreeList()
{
    alias UnboundedFreeList = FreeList!(PureGCAllocator, 0, unbounded);
    alias Allocator = Segregator!(8, FreeList!(PureGCAllocator, 1, 8),
                                  16, FreeList!(PureGCAllocator, 9, 16),
                                  // 128, Bucketizer!(UnboundedFreeList, 1, 128, 16),
                                  // 256, Bucketizer!(UnboundedFreeList, 129, 256, 32),
                                  // 512, Bucketizer!(UnboundedFreeList, 257, 512, 64),
                                  // 1024, Bucketizer!(UnboundedFreeList, 513, 1024, 128),
                                  // 2048, Bucketizer!(UnboundedFreeList, 1025, 2048, 256),
                                  // 3584, Bucketizer!(UnboundedFreeList, 2049, 3584, 512),
                                  PureGCAllocator);
    Allocator allocator;

    immutable nodeCount = 1_000_000;
    immutable wordCount = 4;

    /* store latest pointer here to prevent scoped allocation in clever
     * compilers such as LDC */
    void* latestPtr;

    void testNewAllocation() pure
    {
        auto x = new size_t[wordCount];
        latestPtr = x.ptr;
    }

    void testAllocator() pure
    {
        auto x = allocator.allocate(size_t.sizeof*wordCount);
        latestPtr = x.ptr;
    }

    const results = benchmark!(testNewAllocation, testAllocator)(nodeCount);
    writeln("new-allocation: ", results[0]);
    writeln("std-allocation: ", results[1]);
}

void benchmarkAllocateStrings() @trusted
{
    immutable count = 1_000_000;

    static immutable value = "alpha_beta_gamma_delta";
    immutable(char)* latestPtr;

    void testNewAllocation() @safe pure
    {
        auto x = value.idup;
        latestPtr = &x[0];
    }

    // import core.memory : GC;
    // GC.disable();
    const results = benchmark!(testNewAllocation)(count);
    // GC.enable();
    pragma(msg, typeof(results[0]));
    writefln("Allocating %s strings took %s", count, results[0]);
}

/** Benchmark Project Blizzard safe allocator.
 *
 * See_Also: https://www.youtube.com/watch?v=kaA3HPgowwY&t=1009s
 */
void benchmarkBlizzardSafeAllocator()
{
    alias SafeAllocator = Segregator!(16, AlignedBlockList!(BitmappedBlock!16, AscendingPageAllocator*, 1 << 21),
                                      32, AlignedBlockList!(BitmappedBlock!32, AscendingPageAllocator*, 1 << 21),
                                      64, AlignedBlockList!(BitmappedBlock!64, AscendingPageAllocator*, 1 << 21),
                                      AscendingPageAllocator*);
    SafeAllocator allocator;
    // TODO: this fails: int* i = allocator.make!int(32);
}

void main()
{
    benchmarkAllocatorsRegion();
    benchmarkAllocatorsFreeList();
    benchmarkAllocateStrings();
    benchmarkBlizzardSafeAllocator();
}
