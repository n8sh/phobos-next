/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie
 */
module trie;

import std.traits : isIntegral, isSomeString, allSatisfy;
import std.range : ElementType;

enum isTrieableElementType(T) = isIntegral!T || isSomeChar!T;
enum isTrieType(T) = (isTrieableElementType ||
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
auto radixTreeSet(Value, Keys...)(Keys keys)
{
    return RadixTree!(Value, Keys)(keys);
}

/// Instantiator.
auto radixTreeSet(Keys...)(Keys key)
{
    return RadixTree!(void, Keys)(keys);
}

@safe pure nothrow unittest
{
    auto rt = radixTreeSet!(uint)();
}
