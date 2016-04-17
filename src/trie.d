/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO reuse `UnsignedOfSameSizeAs`
    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

import dbg : dln;

enum isFixedTrieableKeyType(T) = isScalarType!T;

enum isTrieableKey(T) = (isFixedTrieableKeyType!T ||
                         (isInputRange!T &&
                          isFixedTrieableKeyType!(ElementType!T)));

/** Defines how the entries in each `BranchNode` are packed. */
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
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key, Value)
    if (allSatisfy!(isTrieableKey, Key))
{
    import std.algorithm : all;
    import bitop_ex : UnsignedOfSameSizeAs;

    enum isSet = is(Value == void);
    enum hasValue = !isSet;

    enum R = 4;        // radix in number of bits, typically either 1, 2, 4 or 8
    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256
    enum partMask = M - 1;

    alias Br = BranchNode!(M, Value);

    /// Tree depth.
    enum maxDepth = 8*Key.sizeof / R;

    /// `true` if tree has fixed max depth.
    enum isFixed = isFixedTrieableKeyType!Key;

    /// `true` if tree has binary branch.
    enum isBinary = R == 2;

    size_t depth() @safe pure nothrow const
    {
        return root !is null ? root.depth : 0;
    }

    static if (isSet)
    {
        void insert(Key key) @trusted
        {
            makeRoot;

            auto current = root;

            foreach (chunkIx; iota!(0, maxDepth)) // foreach chunk
            {
                enum last = chunkIx == maxDepth - 1;
                enum bitShift = chunkIx*R;
                const u = *(cast(UnsignedOfSameSizeAs!Key*)(&key)); // TODO functionize and reuse here and in intsort.d
                const uint partValue = (u >> bitShift) & partMask; // part of value which is also an index
                assert(partValue < M); // extra range check
                // dln(Key.stringof, " key = ", key, "; chunkIx:", chunkIx, "; partValue:", partValue);

                static if (last) // the last iteration
                {
                }
                else
                {
                    if (current.nexts[partValue] is null)
                    {
                        current.nexts[partValue] = allocateBranch;
                    }
                    current = current.nexts[partValue];
                }
            }
        }
        bool contains(Key key) const
        {
            return true;
        }
    }
    else
    {
        void insert(Key key, Value value)
        {
            makeRoot;
        }
        Value contains(Key key) const
        {
            return Value.init;
        }
    }

    private:

    Br* allocateBranch() @trusted
    {
        auto branch = cast(typeof(return))calloc(1, Br.sizeof);
        assert(branch.nexts[].all!(x => x is null));
        return branch;
    }

    void makeRoot()
    {
        if (root is null) { root = allocateBranch; }
    }

    private Br* root;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Key, Value)() { return RadixTree!(Key, Value)(); }

/// Instantiator.
auto radixTreeSet(Key)() { return RadixTree!(Key, void)(); }

@safe pure nothrow unittest
{
    import std.algorithm : equal;
    struct X { int i; float f; string s; }
    alias Value = X;
    foreach (Key; AliasSeq!(uint))
    {
        auto set = radixTreeSet!(Key);
        auto map = radixTreeMap!(Key, Value);

        static assert(set.isSet);
        static assert(map.hasValue);

        import std.range : iota;
        foreach (e; 0.iota(256))
        {
            const k = cast(Key)e;
            set.insert(k);
            dln(set.depth);
            assert(set.depth == set.maxDepth);
            assert(set.contains(k));
        }

        map.insert(Key.init, Value.init);
    }
}

/** Non-bottom branch node referencing sub-`BranchNode`s or `LeafNode`s. */
private struct BranchNode(size_t M, Value = void)
{
    /// Indicates that only child at this index is occupied.
    static auto oneSet = cast(typeof(this)*)1UL;

    /// Indicates that all children are occupied (typically only for fixed-sized types).
    static auto allSet = cast(typeof(this)*)size_t.max;

    BranchNode!M*[M] nexts;

    size_t depth() @safe pure nothrow const
    {
        import std.algorithm : map, reduce, max;
        return reduce!max(0UL, nexts[].map!(next => next !is null ? next.depth : 0UL)); // TODO replace with fold when switching to 2.071
    }

    static if (!is(Value == void))
    {
        Value value;
    }
}

/** Bottom-most leaf node optionally storing `Value`. */
private struct LeafNode(Value = void)
{
    static if (!is(Value == void))
    {
        Value value;
    }
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
