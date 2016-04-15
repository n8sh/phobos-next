/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO reuse `UnsignedOfSameSizeAs`
    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isArray, allSatisfy;
import std.range : ElementType;

enum isTrieableKeyElementType(T) = isIntegral!T || isSomeChar!T;

enum isTrieableKey(T) = (isTrieableKeyElementType!T ||
                         (isArray!T &&
                          isTrieableKeyElementType!(ElementType!T)));

/** Radix Tree storing keys of types `Keys`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Value, Keys...)
    if (allSatisfy!(isTrieableKey, Keys))
{
    enum isSet = is(Value == void);
    enum hasValue = !isSet;

    static if (isSet)
    {
        void insert(Keys keys)
        {
        }
    }
    else
    {
        void insert(Value value, Keys keys)
        {
        }
    }
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Value, Keys...)()
{
    return RadixTree!(Value, Keys)();
}

/// Instantiator.
auto radixTreeSet(Keys...)()
{
    return RadixTree!(void, Keys)();
}

@safe pure nothrow unittest
{
    struct X { int i; float f; string s; }
    alias Value = X;
    foreach (Key; AliasSeq!(uint, char, string))
    {
        auto set = radixTreeSet!Key();
        set.insert(Key.init);

        auto map = radixTreeMap!(Value, Key)();
        map.insert(Value.init, Key.init);
    }
}
