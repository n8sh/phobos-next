# phobos-next

Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See_Also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

Includes

## Containers
- Trie in `trie.d`.
- Basic uncopyable array with value semantics and explicit copying via `.dup`:
  `basic_array.d`, Wrap in std.typecons.RefCounter for reference semantics.
- Bit array: `bitarray.d`.
- Combined hashset and hashmap with open addressing: `open_hashmap_or_hashset.d`.
- Cyclic arry: `cyclic_array.d`.
- Filter array: `filterarray.d`.
- Statically allocated array similar to C++ `std::static_array`: `fixed_array.d`
- Dynamically allocated (heap) array of fixed length: `fixed_dynamic_array.d`
- A fixed-size (statically stack-allocated) variant of `bitarray.d` in `static_bitarray.d`.
- Minimalistic fixed-length (static) array with length fitting in an `ubyte` for
compact packing: `minimal_fixed_array.d`.
- ...

## Wrapper types
- A wrapper for bounded types: `bound.d`
- Enhanced `NotNull`: `notnull.d`
- A structured wrapper for message digests: `digest_ex.d`
- ...

## Algorithms

- Integer sorting algorithms, including radix sort: `integer_sorting.d`
- Boyer-Moore-Hoorspool search: `horspool.d`
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
