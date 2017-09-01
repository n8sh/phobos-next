module tree;

/// Tree node containing `E`.
private struct Node(E)
{
private:
    E data;

    // allocate with pointers with std.experimental.allocator.makeArray and each
    // pointer with std.experimental.allocator.makeArray
    Node!E*[] subs;
}

/** N-ary tree that cannot shrink but only grow (in breadth and depth).

    Because of this a region allocator can be used for internal memory
    allocation.

    See also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
 */
struct GrowOnlyUpwardsNaryTree(E)
{
    alias N = Node!E;

    /* @safe pure: */
public:
    this(size_t regionSize)
    {
        _allocator = Region!PureMallocator(regionSize);
    }

    this(in E e, size_t regionSize)
    {
        _allocator = Region!PureMallocator(regionSize);
        _root = _allocator.make!N(e);
    }

    /// Returns: top node.
    inout(Node!E)* root() inout
    {
        return _root;
    }

private:
    N* _root;

    import std.conv : emplace;

    // import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : make, makeArray;
    import std.experimental.allocator.building_blocks.region : Region;
    import pure_mallocator : PureMallocator;

    Region!PureMallocator _allocator;
}

unittest
{
    struct X { string src; }

    auto tree = GrowOnlyUpwardsNaryTree!X(X("alpha"), 1024 * 1024);
}
