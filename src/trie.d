/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO Andrei!: SortedRange must be extended to hold these InputRanges aswell.

    TODO Andrei!: Provide RandomAccess opIndex and opSlice! for fixed-length keys?

    TODO Andrei!: Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to root. Add checks with `isSorted`.

    TODO Name members so they indicate that range isSorted.

    TODO Andrei!: Name members to indicate complexity

    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse

    TODO RadixTree: Assigning a void pointer to a class

    I've just started working on a RadixTree implementation in D here:

    https://github.com/nordlow/phobos-next/blob/master/src/trie.d

    The current solution is very generic as it CT-specializes to either a
    `RadixTreeMap` and `RadixTreeSet`. These provide very memory-efficient
    replacements for a `HashMap` and a `HashSet`.

    Each radix-tree branching contains 2^^radix pointers to the next
    sub-branching. In the RadixTreeSet case I want these pointers to also be
    able to contain special "codes" with the following meanings

    0x1: *only* value associated with sub-branching is set.

    0xFFFFFFFFFFFFFFFF: all values values in this sub-branch are set.
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

version = benchmark;

import dbg : dln;

enum isFixedTrieableKeyType(T) = isScalarType!T;

enum isTrieableKey(T) = (isFixedTrieableKeyType!T ||
                         (isInputRange!T &&
                          isFixedTrieableKeyType!(ElementType!T)));

/** Defines how the entries in each `Branch` are packed. */
enum NodePacking
{
    just1Bit,

    sparse2Bit,
    denseBit,

    sparse4Bit,
    dense4Bit,

    sparse8Bit,
    dense8Bit,
}

extern(C) pure nothrow @system @nogc
{
    void* malloc(size_t size);
    void* calloc(size_t nmemb, size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}

/** Radix Tree storing keys of type `Key`.

    In set-case (Value is Void) this containers is especially suitable for
    representing a set of 32 or 64 integers/pointers.

    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key,
                 Value,
                 size_t radix = 4) // radix in number of bits, typically either 1, 2, 4 or 8
    if (allSatisfy!(isTrieableKey, Key))
{
    static assert(radix <= 32, "Radix is currently limited to 32"); // TODO adjust?
    static assert(radix <= 8*Key.sizeof, "Radix must be less than or equal to Key bit-precision"); // TODO Use strictly less than: radix < ... instead?

    import std.algorithm : all;
    import bitop_ex : UnsignedOfSameSizeAs;

    enum isSet = is(Value == void); // `true` if this tree is a set
    enum isMap = !isSet;        // `true` if this tree is a map

    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256
    enum chunkMask = M - 1;

    alias order = M;   // tree order
    alias R = radix;

    alias Br = Branch!(M, Key, Value);

    /// Tree depth.
    enum maxDepth = 8*Key.sizeof / R;

    /// `true` if tree has fixed a key of fixed length and in turn a tree of fixed max depth.
    enum isFixed = isFixedTrieableKeyType!Key;

    /// `true` if tree has binary branch.
    enum isBinary = R == 2;

    size_t depth() @safe pure nothrow const
    {
        return root !is null ? root.linearDepth : 0;
    }

    this(this)
    {
        if (root is null) return;
        auto oldRoot = root;
        makeRoot;
        auto curr = oldRoot;
        while (curr)
        {
            // TODO iterative or recursive?
        }
        assert(false, "TODO traverse tree by branches and leafs and make copies of them");
    }

    ~this()
    {
        if (root) { release(root); }
        debug assert(_branchCount == 0);
    }

    /** Get chunkIndex:th chunk of `radix` number of bits. */
    auto bitsChunk(uint chunkIndex)(Key key) const @trusted pure nothrow
    {
        // calculate bit shift to current chunk
        static if (isIntegral!Key ||
                   isSomeChar!Key) // because top-most bit in ASCII coding (char) is often sparse
        {
            /* most signficant bit chunk first because integers are
               typically more sparse in more significant bits */
            enum shift = (maxDepth - 1 - chunkIndex)*R;
        }
        else
        {
            // default to most signficant bit chunk first
            enum shift = (maxDepth - 1 - chunkIndex)*R;
            // enum shift = chunkIndex*R; // least significant bit first
        }

        const u = *(cast(UnsignedOfSameSizeAs!Key*)(&key)); // TODO functionize and reuse here and in intsort.d
        const uint chunkBits = (u >> shift) & chunkMask; // part of value which is also an index
        static assert(radix <= 8*chunkBits.sizeof, "Need more precision in chunkBits");

        assert(chunkBits < M); // extra range check
        return chunkBits;
    }

    static if (isSet)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(Key key)
        {
            makeRoot;

            auto curr = root;
            foreach (ix; iota!(0, maxDepth)) // NOTE unrolled/inlined compile-time-foreach chunk index
            {
                const chunkBits = bitsChunk!(ix)(key);

                enum isLast = ix + 1 == maxDepth; // if this is the last chunk
                static if (isLast) // this is the last iteration
                {
                    if (curr.nexts[chunkBits] is null)
                    {
                        curr.nexts[chunkBits] = Br.oneSet; // tag this as last
                        return true; // a new value was inserted
                    }
                    else
                    {
                        static if (isFixedTrieableKeyType!Key)
                        {
                            assert(curr.nexts[chunkBits] == Br.oneSet);
                            return false; // value already set
                        }
                        else
                        {
                            // TODO test this
                            if (curr.isOccupied)
                            {
                                return false;
                            }
                            else
                            {
                                curr.isOccupied = false;
                                return true;
                            }
                        }
                    }
                }
                else
                {
                    if (curr.nexts[chunkBits] is null) // if branch not yet visited
                    {
                        curr.nexts[chunkBits] = allocateBranch; // create it
                    }
                    curr = curr.nexts[chunkBits]; // and visit it
                }
            }
            assert(false, "End of function should be reached");
        }

        /** Returns: `true` if key is contained in set, `false` otherwise. */
        bool contains(Key key) const
        {
            if (!root) { return false; }

            auto curr = tailConstRoot;
            foreach (ix; iota!(0, maxDepth)) // NOTE unrolled/inlined compile-time-foreach chunk index
            {
                const chunkBits = bitsChunk!(ix)(key);
                if (curr.nexts[chunkBits] is null)
                {
                    return false;
                }
                else
                {
                    static if (isFixedTrieableKeyType!Key)
                    {
                        if (curr.nexts[chunkBits] == Br.oneSet) { return true; }
                    }
                    else
                    {
                        if (curr.isOccupied)
                        {
                            return false;
                        }
                        else
                        {
                            curr.isOccupied = false;
                            return true;
                        }
                    }
                    curr = curr.nexts[chunkBits];
                }
            }
            return false;
        }
    }
    else
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(Key key, Value value)
        {
            makeRoot;
            return false;
        }

        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(Key key) const
        {
            return null;
        }
    }

    private:

    Br* allocateBranch() @trusted
    {
        auto branch = cast(typeof(return))calloc(1, Br.sizeof);
        debug ++_branchCount;
        assert(branch.nexts[].all!(x => x is null));
        return branch;
    }

    void release(Br* curr) pure nothrow
    {
        foreach (next; curr.nexts)
        {
            if (next &&
                next != Br.oneSet)
            {
                release(next);
            }
        }
        deallocateBranch(curr);
    }

    void deallocateBranch(Br* branch) @trusted
    {
        debug --_branchCount;
        free(cast(void*)branch);  // TODO Allocator.free
    }

    void makeRoot()
    {
        if (root is null) { root = allocateBranch; }
    }

    // TODO is there an existing Phobos function for this?
    const(Br)* tailConstRoot() const pure @trusted nothrow @nogc
    {
        return cast(typeof(return))root;
    }

    /// Returns: number of branches used in `this` tree.
    debug size_t branchCount() @safe pure nothrow @nogc { return _branchCount; }

    private Br* root;
    debug size_t _branchCount = 0;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of radix tree set.
auto radixTreeSet(Key, size_t radix = 4)() { return RadixTree!(Key, void, radix)(); }

/// Instantiator of radix tree map.
auto radixTreeMap(Key, Value, size_t radix = 4)() { return RadixTree!(Key, Value, radix)(); }

auto check()
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        import std.algorithm : equal;
        struct TestValueType { int i; float f; string s; }
        alias Value = TestValueType;
        foreach (Key; AliasSeq!(uint))
        {
            auto set = radixTreeSet!(Key);

            static assert(set.isSet);

            foreach (Key k; 0.iota(512))
            {
                assert(!set.contains(k));
                assert(set.insert(k)); // insert new value returns `true`
                assert(!set.insert(k)); // reinsert same value returns `false`
                assert(set.contains(k));
                assert(set.depth == set.maxDepth);
            }

            debug assert(set.branchCount == 40);

            auto map = radixTreeMap!(Key, Value);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

@safe pure nothrow unittest
{
    check();
}

/// Performance benchmark
version(benchmark) unittest
{
    import core.thread : sleep;
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        import std.algorithm : equal;
        struct TestValueType { int i; float f; string s; }
        alias Value = TestValueType;
        foreach (Key; AliasSeq!(uint))
        {
            auto set = radixTreeSet!(Key);

            static assert(set.isSet);

            import std.conv : to;
            import std.datetime : StopWatch, AutoStart, Duration;
            enum n = 10_000_000;
            auto sw = StopWatch(AutoStart.yes);
            foreach (Key k; 0.iota(n))
            {
                assert(set.insert(k)); // insert new value returns `true`
            }

            dln("Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            sleep(5);
            dln("Sleeping done");

            auto map = radixTreeMap!(Key, Value);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

/** Non-bottom branch node referencing sub-`Branch`s or `Leaf`s. */
private struct Branch(size_t M, Key, Value = void)
{
    enum isMap = !is(Value == void);

    /// `true` if tree has fixed max depth.
    enum isFixed = isFixedTrieableKeyType!Key;

    /// Indicates that only child at this index is occupied.
    static immutable oneSet = cast(typeof(this)*)1UL;

    /// Indicates that all children are occupied (typically only for fixed-sized types).
    // static immutable allSet = cast(typeof(this)*)size_t.max;

    alias Nexts = Branch!(M, Key, Value)*[M];
    Nexts nexts;

    /** Returns: depth of tree at this branch. */
    size_t linearDepth() @safe pure nothrow const
    {
        // TODO replace with fold when switching to 2.071
        import std.algorithm : map, reduce, max;
        return reduce!max(0UL, nexts[].map!(next => (next == oneSet ? 1UL :
                                                     next !is null ? 1 + next.linearDepth :
                                                     0UL)));
    }

    static if (!isFixed)        // variable length keys only
    {
        KeySetLeaf!(M, Key, Value) nextOccupations; // if i:th bit is set key (and optionally value) associated with next[i] is also defined
    }
    static if (isMap)
    {
        Value value;
    }
}

/** Bottom-most leaf node of `RadixTree`-set storing keys of fixed-length type `Key`. */
private struct KeySetLeaf(size_t M, Key, Value = void)
{
    import bitset : BitSet;
    BitSet!M _lastBitChunkDenseMap; // if i:th bit is set corresponding next is set
    alias _lastBitChunkDenseMap this;
}

/** Static Iota.
    TODO Move to Phobos.
 */
template iota(size_t from, size_t to)
    if (from <= to)
{
        alias iota = iotaImpl!(to - 1, from);
}
private template iotaImpl(size_t to, size_t now)
{
    import std.meta : AliasSeq;
    static if (now >= to) { alias iotaImpl = AliasSeq!(now); }
    else                  { alias iotaImpl = AliasSeq!(now, iotaImpl!(to, now + 1)); }
}
