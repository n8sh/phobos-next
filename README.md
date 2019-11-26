# phobos-next

Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See_Also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

Includes

## Containers
- `trie.d`: Trie for prefix completion.
- `basic_array.d`: Basic uncopyable array with value semantics and explicit
  copying via `.dup`.
- Bit array: `bitarray.d`.
- Combined hashset and hashmap with open addressing: `open_hashmap_or_hashset.d`.
- Cyclic arry: `cyclic_array.d`.
- Filter array: `filterarray.d`.
- Statically allocated array similar to C++ `std::array`: `fixed_array.d`
- Dynamically allocated (heap) array of fixed length: `fixed_dynamic_array.d`
- A fixed-size (statically stack-allocated) variant of `bitarray.d` in `static_bitarray.d`.
- Minimalistic fixed-length (static) array with length fitting in an `ubyte` for
compact packing: `minimal_fixed_array.d`.
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

- Debug printing: `dbg.d`
- Symbol regex (structured regular expressions similar to elisps rx): `symbolic.d`
- An n-gram implementation (many nested for loops): `ngram.d`
- Computer Science units: `csunits.d`
- ...

## Extensions
- Various extension to Phobos (often ending with _ex.d)
- ...
