/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isArray, allSatisfy;
import std.range : ElementType;

enum isTrieableElementType(T) = isIntegral!T || isSomeChar!T;
enum isTrieType(T) = (isTrieableElementType!T ||
                      (isArray!T &&
                       isTrieableElementType!(ElementType!T)));

/** Radix Tree storing keys of types `Keys`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Value, Keys...)
    if (allSatisfy!(isTrieType, Keys))
{
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
    alias Value = string;
    foreach (T; AliasSeq!(uint, char))
    {
        auto set = radixTreeSet!T();
        auto map = radixTreeMap!(Value, T)();
    }
}
