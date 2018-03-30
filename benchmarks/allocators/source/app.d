import std.stdio;
import std.experimental.allocator;
import std.experimental.allocator.building_blocks;

void testAllocators()
{
    alias FList = FreeList!(GCAllocator, 0, unbounded);
    alias A = Segregator!(
        8, FreeList!(GCAllocator, 1, 8),
        16, FreeList!(GCAllocator, 9, 16),
        128, Bucketizer!(FList, 1, 128, 16),
        // 256, Bucketizer!(FList, 129, 256, 32),
        // 512, Bucketizer!(FList, 257, 512, 64),
        // 1024, Bucketizer!(FList, 513, 1024, 128),
        // 2048, Bucketizer!(FList, 1025, 2048, 256),
        // 3584, Bucketizer!(FList, 2049, 3584, 512),
        GCAllocator
        );
    A a;

    immutable n = 50_000_000;
    foreach (i; 0 .. n)
    {
        // auto x = new size_t[16];
        // auto x = a.allocate(128);
    }
}

void main()
{
    testAllocators();
}
