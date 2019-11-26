# phobos-next

Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See_Also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

Includes

## Containers
- `trie.d`: Trie for prefix completion.
- `basic_array.d`: Basic uncopyable array with value semantics and explicit
  copying via `.dup`.
- `bitarray.d`: Bit array.
- `open_hashmap_or_hashset.d`: Combined hashset and hashmap with open addressing.
- `cyclic_array.d`: Cyclic arry.
- `filterarray.d`: Filter array.
- `fixed_array.d`: Statically allocated array similar to C++ `std::array`.
- `fixed_dynamic_array.d`: Dynamically allocated (heap) array of fixed length.
- `bitarray.d`: A dynamically-sized (heap) bit array.
- `static_bitarray.d`: A statically-size (stack) bit array.
- `minimal_fixed_array.d`: Minimalistic statically-sized (stack) array of length smaller
than 255 fitting in an `ubyte` for compact packing.
- ...

For reference semantics wrap uncopyable containers in `std.typecons.RefCounter`.

## Wrapper types
- `bound.d`: A wrapper for bounded types.
- `notnull.d`: Enhanced `NotNull`.
- `digest_ex.d`: A structured wrapper for message digests.
- ...

## Algorithms

- `integer_sorting.d`: Integer sorting algorithms, including radix sort.
- `horspool.d`: Boyer-Moore-Hoorspool search.
- ...

## Utilities

- `dbg.d`: Debug printing.
- `symbolic.d`: Symbol regex (structured regular expressions similar to elisps rx).
- `ngram.d`: An n-gram implementation (many nested for loops).
- `csunits.d`: Computer Science units.
- ...

## Extensions
- Various extension to Phobos (often ending with _ex.d)
- ...
