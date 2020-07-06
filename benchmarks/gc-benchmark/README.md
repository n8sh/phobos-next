# Sweep-Free Segregated GC for the D programming language (Dlang)

This project contains a specification and implementation of a new garbage
collector for the D programming language.

## Specification

### Segregated by pool type

Opposite to D's current GC, different type classes are allocated in separate
pools (segregated allocation). This will lead to worse cache locality during
consecutive allocation of instances of different (size) types. The
implementation will however be significantly simpler especially when D's design
by introspection via `static foreach` plus `mixin` is utilized to implement
different pool types. Segregation happens on all combinations of

- size class,
- scanningness: (whether they may contain pointers or not), and
- whether they contain finalizers or now (`struct` or `class`)

resulting in `2*2*number_of_size_classes` different pool kinds. This is matches
Dmitry Olshansky recommendations for a new GC in his blog post titled "Inside
D's GC" which is currently missing from the web. A copy is hosted [locally
here](./inside-d-gc-by-dmitry-olshansky.md).

### Uses compile-time introspection

GC is uses `static foreach` plus `mixin` to construct and use instances of these
pool types for different size classes with minimal code duplication.

### Densely stored mark bits and sweep-free

GC is inspired by Go's [Proposal: Dense mark bits and sweep-free
allocation](https://github.com/golang/proposal/blob/master/design/12800-sweep-free-alloc.md)
also reference [here](https://github.com/golang/go/issues/12800). This spec
makes use of two continuous bitmaps, called `slotUsages` is used during
allocation phase. During mark-phase an equally sized bitmap, `slotMarks`, is
zero-initialized and filled in as pointers to slots are discovered to be
referenced. When mark-phase is complete this new bitmap `slotMarks` is swapped
with `slotUsages`. This may or may not work for pools of objects that have
finalizers (TODO find out).

When the allocator has grown too large it will be neccessary to indeed do
sweeps to free pages. But such sweeps can be triggered by low memory and
doesn't have to do a complete sweep if low latency is needed.

### Choice of Size Classes

Use jemalloc `size classes`: For size classes in between powers of two we can
allocate pages in 3*n chunks. This is has been added to D's default GC aswell.

Calculate size class at compile-time using next power of 2 of `T.sizeof` for
calls to `new T()` and feed into `N` size-dependent overloads of `mallocN()`,
`callocN()`, `reallocN()` etc.

For each pool use a hash-table from base pointer to page index instead of a
binary search to speed up page-search ([Olshansky
again](./inside-d-gc-by-dmitry-olshansky.md)). Use hash-table with open
addressing and Fibonacci hashing, for instance, phobos-next's
[`open_hashmap_or_hashset.c`](https://github.com/nordlow/phobos-next/blob/master/src/open_hashmap_or_hashset.d). This
hash-table needs to be cleared up after mark phase or during allocation.

Add run-time information for implicit (by compiler) and explicit (by developer
in library) casting from mutable to `immutable` and, in turn, `shared` for
isolated references.  Typically named: `__cast_immutable`, `__cast_shared`. To
make this convenient the compiler might ahead-of-time calculate figure out if
non-`shared` allocation later must be treated as `shared` and allocated in the
first place on the global GC heap.

### Mark-phase

- For each potential pointer `p` in stack
  - Check if `p` lies within address bounds of all pools. This phase might be
    integrated with a hash-table lookup of the block part of the pointer.
  - If so, find page storing that pointer (using a hashmap from base pointers to pages)
  - If that slot lies in a pool and and that slot belongs to a pool whols
    element types may contain pointers that slot hasn't yet been marked scan that
    slot.
- Finally mark slot
- Find first free slot (0) in pageSlotOccupancies bitarray of length using
  `core.bitop`. Use my own bitarray implementation.

## Key-Questions

- Should slot occupancy status

1. be explicitly stored in a bitarray and allocated in conjunction with
pages somehow (more performant for dense representations) This requires this
bitarray to be dynamically expanded and deleted in-place when pages are
removed
2. automatically deduced during sweep into a hashset of pointers (more
performant for sparse data) and keep some extra

### Conservative for now, Precise later

GC is conservative for now but will be prepared for a merge with Rainer's
precise add-ons.

## Note

Please note that block attribute data must be tracked, or at a minimum, the
FINALIZE bit must be tracked for any allocated memory block because calling
rt_finalize on a non-object block can result in an access violation.  In the
allocator below, this tracking is done via a leading uint bitmask.  A real
allocator may do better to store this data separately, similar to the basic GC.

## References

1. [Inside D's GC](https://olshansky.me/gc/runtime/dlang/2017/06/14/inside-d-gc.html)

2. [DIP 46: Region Based Memory Allocation](https://wiki.dlang.org/DIP46)

3. [Thread-local GC](https://forum.dlang.org/thread/xiaxgllobsiiuttavivb@forum.dlang.org)

4. [Thread GC non "stop-the-world"](https://forum.dlang.org/post/dnxgbumzenupviqymhrg@forum.dlang.org)

5. [Conservative GC: Is It Really That Bad?](https://www.excelsiorjet.com/blog/articles/conservative-gc-is-it-really-that-bad/)
   and [here](https://forum.dlang.org/thread/qperkcrrngfsbpbumydc@forum.dlang.org)

6. [GC page and block metadata storage](https://forum.dlang.org/thread/fvmiudfposhggpjgtluf@forum.dlang.org)

7. [Scalable memory allocation using jemalloc](https://www.facebook.com/notes/facebook-engineering/scalable-memory-allocation-using-jemalloc/480222803919/)

8. [How does jemalloc work? What are the benefits?](https://stackoverflow.com/questions/1624726/how-does-jemalloc-work-what-are-the-benefits)

9. [What are the advantages and disadvantages of having mark bits together and separate for Garbage Collection](https://stackoverflow.com/questions/23057531/what-are-the-advantages-and-disadvantages-of-having-mark-bits-together-and-separ)

10. [Adding your own GC to the GC Registry](https://dlang.org/spec/garbage.html#gc_registry)
