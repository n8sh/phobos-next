# phobos-next

Additional useful containers, algorithms, wrapper types, traits etc. Several are
generic enough to have a place in Phobos.

Documentation generated [here](https://phobos-next.dpldocs.info/index.html).

Announcement [here](http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org).

Includes

## Containers
- `trie.d`: Trie for prefix completion(s).
- `dynamic_array.d`: Basic uncopyable array with value semantics and explicit
  copying via `.dup`.
- `fixed_dynamic_array.d`: Dynamically allocated (heap) array with fixed length.
- `minimal_fixed_array.d`: Minimalistic statically-sized (stack) array of length smaller
than 255 fitting in an `ubyte` for compact packing.
- `open_hashmap_or_hashset.d`: Combined hashset and hashmap with open addressing
  and support for deletion via hole handling. Pointers and classes are stored as
  is with support for vacancy and hole handling. Vacancy support for
  `std.typecons.Nullable`.
- `cyclic_array.d`: Cyclic array.
- `filterarray.d`: Filter array.
- `fixed_array.d`: Fixed-sized statically allocated (heap) array similar to C++ `std::array`.
- `fixed_dynamic_array.d`: Fixed-sized heap-allocated array.
- `bitarray.d`: A dynamically sized (heap) bit array.
- `static_bitarray.d`: A statically sized (stack) bit array.
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
