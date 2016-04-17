/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO reuse `UnsignedOfSameSizeAs`
    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

enum isFixedTrieableKeyType(T) = isIntegral!T || isSomeChar!T;

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

/** Radix Tree storing keys of type `Key`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key, Value)
    if (allSatisfy!(isTrieableKey, Key))
{
    enum isSet = is(Value == void);
    enum hasValue = !isSet;

    enum R = 4;        // radix in number of bits, typically either 1, 2, 4 or 8
    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256

    /// `true` if tree has fixed depth.
    enum isFixed = isFixedTrieableKeyType!Key;

    /// `true` if tree has binary branch.
    enum isBinary = R == 2;

    static if (isSet)
    {
        void insert(Key key)
        {
            auto current = _root;
            enum maxDepth = 8*Key.sizeof / R;
            foreach (i; iota!(0, maxDepth)) // static
            {
                enum bitShift = i*R;
                pragma(msg, bitShift);
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
            auto current = _root;
        }
        Value contains(Key key) const
        {
            return Value.init;
        }
    }

    private BranchNode!(M, Value)* _root;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Key, Value)() { return RadixTree!(Key, Value)(); }

/// Instantiator.
auto radixTreeSet(Key)() { return RadixTree!(Key, void)(); }

@safe pure nothrow unittest
{
    struct X { int i; float f; string s; }
    alias Value = X;
    foreach (Key; AliasSeq!(ubyte))
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
            assert(set.contains(k));
        }

        map.insert(Key.init, Value.init);
    }
}

/** Non-Bottom (Leaf) Node referencing sub-`BranchNode`s or `LeafNode`s. */
struct BranchNode(size_t M, Value = void)
{
    /// Indicates that only child at this index is occupied.
    static BranchNode!M* oneSet = cast(BranchNode!M*)1;

    /// Indicates that all children are occupied (typically only for fixed-sized types).
    static BranchNode!M* allSet = cast(BranchNode!M*)size_t.max;

    BranchNode!M*[M] nexts;
}

/** Bottom-Most Leaf Node optionnally storing `Value`. */
struct LeafNode(Value = void)
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
