/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO reuse `UnsignedOfSameSizeAs`
    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

enum isTrieableKeyElementType(T) = isIntegral!T || isSomeChar!T;

enum isTrieableKey(T) = (isTrieableKeyElementType!T ||
                         (isInputRange!T &&
                          isTrieableKeyElementType!(ElementType!T)));

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

/** Radix Tree storing keys of type `Key`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key, Value)
    if (allSatisfy!(isTrieableKey, Key))
{
    enum isSet = is(Value == void);
    enum hasValue = !isSet;

    enum radix = 4;             // radix in number of bits
    enum N = 2^^radix;

    /// Check if tree has fixed depth.
    enum isFixed = isTrieableKeyElementType!Key;

    enum isBinary = radix == 2;

    static if (isSet)
    {
        void insert(Key key)
        {
            auto current = _root;
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
            auto current = _root;
        }
        Value contains(Key key) const
        {
            return Value.init;
        }
    }

    private BranchNode!(N, Value)* _root;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Key, Value)() { return RadixTree!(Key, Value)(); }

/// Instantiator.
auto radixTreeSet(Key)() { return RadixTree!(Key, void)(); }

/** Non-Bottom (Leaf) Node referencing sub-`BranchNode`s or `LeafNode`s. */
struct BranchNode(size_t N, Value = void)
{
    /// Indicates that only child at this index is occupied.
    static BranchNode!N* oneSet = cast(BranchNode!N*)1;

    /// Indicates that all children are occupied (typically only for fixed-sized types).
    static BranchNode!N* allSet = cast(BranchNode!N*)size_t.max;

    BranchNode!N*[N] nexts;
}

/** Bottom-Most Leaf Node optionnally storing `Value`. */
struct LeafNode(size_t N, Value = void)
{
    static if (!is(Value == void))
    {
        Value value;
    }
}

@safe pure nothrow unittest
{
    struct X { int i; float f; string s; }
    alias Value = X;
    foreach (Key; AliasSeq!(char, uint))
    {
        auto set = radixTreeSet!(Key);
        set.insert(Key.init);
        assert(set.contains(Key.init));

        auto map = radixTreeMap!(Key, Value);
        map.insert(Key.init, Value.init);
    }
}
