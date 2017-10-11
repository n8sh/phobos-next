module hashmap;

import container_traits;

enum InsertionStatus { added, modified, unchanged }

/** Hash set (or map) storing (key) elements of type `K` and values of type `V`.
 *
 * Uses small-size-optimized (SSO) arrays as buckets, which provides more stable
 * behaviour than open-addressing.
 *
 * Params:
 *      K = key type.
 *      V = value type.
 *      Allocator = memory allocator for bucket array and large buckets (`LargeBucket`)
 *      hasher = hash function or std.digest Hash.
 *      smallBucketMinCapacity = minimum capacity of small bucket
 *
 * See also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 *
 * TODO add flag for use growth factor smaller than powers of two. Using prime
 * table. Details are here: https://github.com/greg7mdp/sparsepp
 *
 * TODO add withElements() with fast pre-allocation `withCapacity`
 *
 * TODO use core.bitop : bsr, bsl to find first empty element in bucket
 *
 * TODO Avoid extra length and capacity in _statuses (length or large) by making
 * it allocate in sync with buckets (using soa.d)
 *
 * TODO rehash: if allocator has realloc we can do rehashing in-place similar to
 * reordering in in-place radix (integer_sorting.d), otherwise rehash into new
 * copy of buckets and free old buckets when done. If bucket element count is >
 * 1 this is more complicated since each bucket contains a set of elements to
 * swap out and must be put in a queue.
 *
 * TODO forward-ranges `byValue`, `byKeyValue`
 *
 * TODO support uncopyable value type for map-case
 *
 * TODO benchmark against https://github.com/greg7mdp/sparsepp
 */
struct HashMapOrSet(K, V = void,
                    alias Allocator = null,
                    alias hasher = hashOf,
                    uint smallBucketMinCapacity = 1)
    if (smallBucketMinCapacity >= 1) // no use having empty small buckets
{
    import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor;
    import std.algorithm.mutation : move, moveEmplace, moveEmplaceAll;
    import std.algorithm.searching : canFind, countUntil;
    import std.conv : emplace;
    import hash_ex : HashOf;
    import prime_modulo;

    /** In the hash map case, `V` is non-void, and a value is stored alongside
     * the key of type `K`.
     */
    enum hasValue = !is(V == void);

    /** Type of key stored. */
    alias KeyType = K;

    /// Element type.
    static if (hasValue)
    {
        struct T
        {
            K key;
            V value;
        }

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element.key;
        }

        /// Get reference to key part of `element`.
        static ref inout(K) keyRefOf()(ref return inout(T) element)
        {
            return element.key;
        }

        /// Get value part of element.
        static auto ref inout(V) valueOf()(auto ref return inout(T) element)
        {
            return element.value;
        }

        alias ValueType = V;

        enum keyEqualPred = "a.key == b";
    }
    else                        // HashSet
    {
        private alias T = K;

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element;
        }

        /// Get reference to key part of `element`.
        static ref inout(K) keyRefOf()(ref return inout(T) element)
        {
            return element;
        }

        enum keyEqualPred = "a == b";
    }

    alias ElementType = T;

    /** Make with room for storing at least `capacity` number of elements.
     */
    pragma(inline)              // LDC can, DMD cannot inline
    static typeof(this) withCapacity(size_t capacity)
    {
        return typeof(return)(capacity);
    }

    pragma(inline)              // LDC can, DMD cannot inline
    private static typeof(this) withBucketCount(size_t bucketCount)
    {
        // TODO return direct call to store constructor
        typeof(return) that;

        that._buckets = Buckets.withLength(bucketCount);
        that._bstates = Bstates.withLength(bucketCount);

        that._length = 0;
        return that;
    }

    /** Construct with room for storing at least `capacity` number of elements.
     */
    private this(size_t capacity)
    {
        const minimumBucketCount = capacity / smallBucketCapacity;
        import std.math : nextPow2;

        // make bucket count a power of two
        immutable bucketCount = nextPow2(minimumBucketCount == 0 ?
                                         0 :
                                         minimumBucketCount - 1);

        // initialize buckets and states
        _buckets = Buckets.withLength(bucketCount);
        _bstates = Bstates.withLength(bucketCount);

        assert(_bstates.length == bucketCount);
        _length = 0;
    }

    /// Destruct.
    ~this()
    {
        release();
    }

    /// No copying.
    @disable this(this);

    /// Duplicate.
    typeof(this) dup() @trusted
    {
        typeof(return) that;

        that._buckets.reserve(_buckets.length);

        // TODO merge these
        that._buckets.length = _buckets.length; // TODO this zero-initializes before initialization below, use unsafe setLengthOnlyUNSAFE
        that._bstates = _bstates.dup;

        that._length = _length;

        foreach (immutable bucketIx; 0 .. _buckets.length)
        {
            if (_bstates[bucketIx].isLarge)
            {
                emplace(&that._buckets[bucketIx].large,
                        _buckets[bucketIx].large[]);
            }
            else
            {
                /** TODO functionize to `emplaceAll`. See also:
                 * http://forum.dlang.org/post/xxigbqqflzwfgycrclyq@forum.dlang.org
                 */
                static if (hasElaborateCopyConstructor!T)
                {
                    foreach (immutable elementIx, const ref element; elementsOfSmallBucket(bucketIx))
                    {
                        emplace(&that._buckets[bucketIx].small[elementIx],
                                element);
                    }
                }
                else
                {
                    enum useMemcpy = true;
                    static if (useMemcpy)
                    {
                        // currently faster than slice assignment on else branch
                        import core.stdc.string : memcpy;
                        memcpy(that._buckets[bucketIx].small.ptr, // cannot overlap
                               elementsOfSmallBucket(bucketIx).ptr,
                               elementsOfSmallBucket(bucketIx).length * T.sizeof);
                    }
                    else
                    {
                        that._buckets[bucketIx].small.ptr[0 .. _bstates[bucketIx].smallCount] = elementsOfSmallBucket(bucketIx);
                    }
                }
            }
        }

        return that;
    }

    /// Grow by duplicating number of buckets.
    private void grow() @trusted
    {
        immutable newBucketCount = bucketCount ? 2 * bucketCount : 1; // 0 => 1, 1 => 2, 2 => 4, ...
        auto copy = withBucketCount(newBucketCount);

        foreach (immutable bucketIx; 0 .. _buckets.length)
        {
            foreach (const ref element; bucketElementsAt(bucketIx))
            {
                copy.insertWithoutGrowth(element);
            }
        }

        assert(copy._length == _length); // length shouldn't change

        // do the growth in-place
        moveEmplace(copy._bstates, _bstates);
        move(copy._buckets, _buckets);

        assert(!_buckets.empty);
        assert(!_bstates.empty);
    }

    /// Equality.
    bool opEquals(in ref typeof(this) rhs) const @trusted
    {
        if (_length != rhs._length) { return false; }
        foreach (immutable bucketIx; 0 .. _buckets.length)
        {
            foreach (const ref element; bucketElementsAt(bucketIx))
            {
                if (!rhs.contains(element)) { return false; }
            }
        }
        return true;
    }

    /// Empty.
    void clear()
    {
        release();
        resetInternalData();
    }

    /// Release internal store.
    private void release() @trusted
    {
        foreach (immutable bucketIx; 0 .. _buckets.length)
        {
            if (_bstates[bucketIx].isLarge)
            {
                static if (hasElaborateDestructor!LargeBucket)
                {
                    .destroy(_buckets[bucketIx].large);
                }
            }
            else
            {
                /* TODO SmallBucket itself doens't need to be destroyed only
                   it's elements and gc_removeRange doesn't need to be called
                   either, that is take car of by dtor of _buckets. */
                static if (hasElaborateDestructor!SmallBucket)
                {
                    .destroyAll(elementsOfSmallBucket(bucketIx));
                }
            }
        }
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        _buckets.clear();
        _bstates.clear();
        _length = 0;
    }

    /** Check if `element` is stored.
        Returns: `true` if element was already present, `false` otherwise.
     */
    bool contains(in T element) const @trusted
    {
        if (empty)
        {
            // prevent range error in `bucketElementsAt` when `this` is empty
            return false;
        }
        immutable bucketIx = keyToBucketIx(keyRefOf(element));
        return bucketElementsAt(bucketIx).canFind(element);
    }

    /** Insert `element`, being either a key, value (map-case) or a just a key (set-case).
     */
    InsertionStatus insert(T element)
    {
        if (_length + 1 > _buckets.length * smallBucketCapacity)
        {
            grow();
        }
        return insertWithoutGrowth(element);
    }

    /** Insert `element` like with `insert()` but without automatic growth.
     */
    InsertionStatus insertWithoutGrowth(T element) @trusted
    {
        immutable bucketIx = keyToBucketIx(keyRefOf(element));
        T[] bucketElements = bucketElementsAt(bucketIx);

        immutable ptrdiff_t elementOffset = bucketElements.countUntil!keyEqualPred(keyOf(element));
        immutable hit = elementOffset != -1;
        if (hit)
        {
            static if (hasValue) // replace value
            {
                if (bucketElements[elementOffset].value != valueOf(element))
                {
                    bucketElements[elementOffset].value = valueOf(element); // replace valae
                    return typeof(return).modified;
                }
            }
            return typeof(return).unchanged;
        }
        else                    // no hit
        {
            if (_bstates[bucketIx].isLarge) // stay large
            {
                _buckets[bucketIx].large.insertBackMove(element);
            }
            else
            {
                if (_bstates[bucketIx].isFullSmall) // expand small to large
                {
                    SmallBucket smallCopy = _buckets[bucketIx].small;
                    static assert(!hasElaborateDestructor!T, "moveEmplaceAll small elements to large");
                    emplace!(LargeBucket)(&_buckets[bucketIx].large,
                                          smallCopy[0 .. _bstates[bucketIx].smallCount]);
                    _buckets[bucketIx].large.insertBackMove(element);
                    _bstates[bucketIx].makeLarge();
                }
                else            // stay small
                {
                    _buckets[bucketIx].small[_bstates[bucketIx].smallCount] = element;
                    _bstates[bucketIx].incSmallCount();
                }
            }
            _length += 1;
            return typeof(return).added;
        }
    }

    /** Element reference (and in turn range iterator). */
    static private struct ElementRef
    {
        HashMapOrSet* table;
        size_t bucketIx;        // index to bucket inside table
        size_t elementOffset;   // offset to element inside bucket

        bool opCast(T : bool)() const
        {
            return table !is null;
        }

        scope ref inout(T) opUnary(string s)() inout return
            if (s == "*")
        {
            assert(table);
            return table.bucketElementsAt(bucketIx)[elementOffset];
        }
    }

    /// ditto
    static if (!hasValue)       // HashSet
    {
        bool opBinaryRight(string op)(in K key) inout @trusted
            if (op == "in")
        {
            return contains(key);
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(ElementRef) opBinaryRight(string op)(in K key) inout @trusted return
            if (op == "in")
        {
            if (empty)
            {
                // prevent range error in `bucketElementsAt` when `this` is empty
                return typeof(return).init;
            }
            immutable bucketIx = keyToBucketIx(key);
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIx).countUntil!(_ => _.key == key); // TODO functionize
            immutable hit = elementOffset != -1;
            if (hit)
            {
                return typeof(return)(&this, bucketIx, elementOffset);
            }
            else                    // miss
            {
                return typeof(return).init;
            }
        }

        static private struct ByKey // TODO scope
        {
            @property bool empty() const
            {
                return bucketIx == table.bucketCount;
            }

            @property auto front() inout
            {
                return table.bucketElementsAt(bucketIx)[elementOffset].key;
            }

            void initFirstNonEmptyBucket()
            {
                while (bucketIx < table.bucketCount &&
                       table.bucketElementCountAt(bucketIx) == 0)
                {
                    bucketIx += 1;
                }
            }

            void popFront()
            {
                assert(!empty);
                elementOffset += 1; // next element
                // if current bucket was emptied
                while (elementOffset >= table.bucketElementsAt(bucketIx).length)
                {
                    // next bucket
                    bucketIx += 1;
                    elementOffset = 0;
                    if (empty) { break; }
                }
            }

            @property typeof(this) save() // ForwardRange
            {
                return this;
            }

            private ElementRef _elementRef;  // range iterator, TODO alias this
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys.
        inout(ByKey) byKey() inout @trusted return
        {
            auto result = typeof(return)(inout(ElementRef)(&this));
            (cast(ByKey)result).initFirstNonEmptyBucket(); // dirty cast because inout problem
            return result;
        }

        /// Indexing.
        scope ref inout(V) opIndex(in K key) inout return
        {
            immutable bucketIx = keyToBucketIx(key);
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIx).countUntil!(_ => _.key == key); // TODO functionize
            immutable hit = elementOffset != -1;
            if (hit)
            {
                return bucketElementsAt(bucketIx)[elementOffset].value;
            }
            else                    // miss
            {
		import std.conv : text;
                import core.exception : RangeError;
                throw new RangeError("Key " ~ text(key) ~ " not in table");
            }
        }

        /** Get value of `key` or `defaultValue` if `key` not present (and
         * therefore `nothrow`).
         *
         * TODO make `defaultValue` `lazy` when that can be `nothrow`
         */
        V get(in K key, V defaultValue) @trusted
        {
            immutable bucketIx = keyToBucketIx(key);
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIx).countUntil!(_ => _.key == key); // TODO functionize
            if (elementOffset != -1) // hit
            {
                return bucketElementsAt(bucketIx)[elementOffset].value;
            }
            else                    // miss
            {
                return defaultValue;
            }
        }

	/** Supports $(B aa[key] = value;) syntax.
	 */
	void opIndexAssign(V value, K key)
	{
            insert(T(key, value));
	}

    }

    /** Remove `element` and, when possible, shrink its large bucket to small.

        Returns: `true` if element was removed, `false` otherwise.
    */
    bool remove(in K key)
        @trusted
    {
        immutable bucketIx = keyToBucketIx(key);
        import container_algorithm : popFirstMaybe;
        if (_bstates[bucketIx].isLarge)
        {
            immutable hit = _buckets[bucketIx].large.popFirstMaybe!keyEqualPred(key);
            _length -= hit ? 1 : 0;
            if (hit)
            {
                tryShrinkLargeBucketAt(bucketIx);
            }
            return hit;
        }
        else
        {
            immutable elementIx = elementsOfSmallBucket(bucketIx).countUntil!keyEqualPred(key);
            immutable hit = elementIx != -1;
            if (hit)
            {
                removeSmallElementAt(bucketIx, elementIx);
            }
            _length -= hit ? 1 : 0;
            return hit;
        }
    }

    /** Remove small element at `elementIx` in bucket `bucketIx`. */
    private void removeSmallElementAt(size_t bucketIx,
                                      size_t elementIx)
    {
        assert(!_bstates[bucketIx].isLarge);
        import container_algorithm : shiftToFrontAt;
        elementsOfSmallBucket(bucketIx).shiftToFrontAt(elementIx);
        _bstates[bucketIx].decSmallCount();
        static if (hasElaborateDestructor!T)
        {
            .destroy(_buckets[bucketIx].small[_bstates[bucketIx].smallCount]);
        }
    }

    /** Shrink large bucket at `bucketIx` possible posbiel. */
    private void tryShrinkLargeBucketAt(in size_t bucketIx)
    {
        assert(_bstates[bucketIx].isLarge);
        immutable count = _buckets[bucketIx].large.length;
        if (count <= smallBucketCapacity) // large fits in small
        {
            static assert(!hasElaborateDestructor!T);

            SmallBucket small; // TODO = void
            moveEmplaceAll(_buckets[bucketIx].large[], small[0 .. count]);

            moveEmplaceAll(small[0 .. count], _buckets[bucketIx].small[0 .. count]);

            _bstates[bucketIx].smallCount = count;
        }
    }

    /** Rehash.
     *
     * Reorganize `this` in place so that lookups are more efficient.
     */
    ref typeof(this) rehash()() @trusted
    {
        static assert(false, "TODO remove template parens of this functions and implement");
        // return this;
    }

    /// Check if empty.
    @property bool empty() const { return _length == 0; }

    /// Get length (read-only).
    @property size_t length() const { return _length; }

    /// Get bucket count.
    @property size_t bucketCount() const { return _buckets.length; }

    /// Bucket count statistics.
    struct BucketCounts
    {
        size_t smallCount;      // number of hybrid buckets being small
        size_t largeCount;      // number of hybrid buckets being large
    }

    /// Get bucket count statistics.
    BucketCounts bucketCounts() const
    {
        import std.algorithm : count;
        immutable largeCount = _bstates[].count!(_ => _.isLarge);
        immutable smallCount = _bstates.length - largeCount;
        auto result = typeof(return)(smallCount,
                                     largeCount);
        assert(result.largeCount + result.smallCount == _bstates.length);
        return result;
    }

    /** Returns: elements in bucket at `bucketIx`. */
    pragma(inline, true)
    private scope inout(T)[] bucketElementsAt(size_t bucketIx) inout return
    {
        if (_bstates[bucketIx].isLarge)
        {
            return _buckets[bucketIx].large[];
        }
        else
        {
            return elementsOfSmallBucket(bucketIx);
        }
    }

    pragma(inline, true)
    private scope inout(T)[] elementsOfSmallBucket(size_t bucketIx) inout return
    {
        return _buckets[bucketIx].small[0 .. _bstates[bucketIx].smallCount];
    }

    /** Returns: number of elements in bucket at `bucketIx`. */
    pragma(inline, true)
    private size_t bucketElementCountAt(size_t bucketIx) const
    {
        if (_bstates[bucketIx].isLarge)
        {
            return _buckets[bucketIx].large.length;
        }
        else
        {
            return _bstates[bucketIx].smallCount;
        }
    }

private:
    import basic_uncopyable_array : Array = UncopyableArray;

    /** 32-bit capacity and length for LargeBucketLnegth on 64-bit platforms
     * saves one word and makes insert() and contains() significantly faster */
    alias LargeBucketCapacityType = uint;
    alias LargeBucket = Array!(T, Allocator, LargeBucketCapacityType);

    import std.algorithm : max;
    enum smallBucketCapacity = max(smallBucketMinCapacity,
                                   LargeBucket.sizeof / T.sizeof);

    alias SmallBucket = T[smallBucketCapacity];

    /// no space for `SmallBucket`s should be wasted
    static assert(SmallBucket.sizeof >= LargeBucket.sizeof);

    /** Small-size-optimized bucket.
     */
    union HybridBucket
    {
        LargeBucket large;
        SmallBucket small;
    }

    /** Small-size-optimized bucket array.

        Size-state (including small or large) for each element is determined by
        corresponding element in `Bstates`.
     */
    alias Buckets = Array!(HybridBucket, Allocator);

    /** Count and large status of bucket. */
    struct Bstate
    {
        static if (smallBucketCapacity <= 7)
        {
            alias Count = ubyte;
        }
        else
        {
            alias Count = ushort;
        }

        pragma(inline, true):

        @property Count smallCount() const
        {
            assert(_count <= smallBucketCapacity);
            return _count;
        }

        @property void smallCount(size_t count)
        {
            assert(count <= smallBucketCapacity);
            _count = cast(Count)count;
        }

        @property void decSmallCount()
        {
            assert(_count >= 1);
            _count -= 1;
        }

        @property void incSmallCount()
        {
            assert(_count + 1 <= smallBucketCapacity);
            _count += 1;
        }

        /** Returns: `true` iff `this` is a large bucket. */
        @property bool isLarge() const
        {
            return _count == Count.max;
        }

        @property void makeLarge()
        {
            _count = Count.max;
        }

        /** Returns: `true` iff `this` is a full small bucket. */
        @property bool isFullSmall() const
        {
            return _count == smallBucketCapacity;
        }

        Count _count;
    }

    alias Bstates = Array!(Bstate, Allocator);

    // TODO merge these into an instance of soa.d and remove invariant
    Buckets _buckets;
    Bstates _bstates;
    invariant
    {
        assert(_buckets.length ==
               _bstates.length);
    }

    size_t _length;

    /** Returns: bucket index of `hash`. */
    pragma(inline, true)
    size_t hashToIndex(size_t hash) const
    {
        const size_t mask = _buckets.length - 1;
        assert((~mask ^ mask) == size_t.max); // assert that _buckets.length is a power of 2
        return hash & mask;
    }

    /** Returns: bucket index of `key`. */
    pragma(inline, true)
    size_t keyToBucketIx()(in auto ref K key) const
    {
        return hashToIndex(HashOf!(hasher)(key));
    }

}

/** Hash map storing keys of type `K`.
 */
alias HashSet(K,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBucketMinCapacity = 1) = HashMapOrSet!(K, void,
                                                               Allocator,
                                                               hasher,
                                                               smallBucketMinCapacity);

/** Hash map storing keys of type `K` and values of type `V`.
 */
alias HashMap(K, V,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBucketMinCapacity = 1) = HashMapOrSet!(K, V,
                                                               Allocator,
                                                               hasher,
                                                               smallBucketMinCapacity);

@safe pure nothrow @nogc unittest
{
    import digestx.fnv : FNV;

    immutable n = 11111;

    alias K = uint;

    foreach (V; AliasSeq!(void, string))
    {
        alias X = HashMapOrSet!(K, V, null, FNV!(64, true));

        import container_traits : mustAddGCRange;
        static if (X.hasValue)
        {
            static assert(!mustAddGCRange!(X.LargeBucket));
            static assert(mustAddGCRange!(X.T));
            // TODO static assert(mustAddGCRange!(X.SmallBucket));
        }
        else
        {
            static assert(!mustAddGCRange!(X.T));
            static assert(!mustAddGCRange!(X.SmallBucket));
        }

        auto x1 = X();            // start empty

        // all buckets start small
        assert(x1.bucketCounts.largeCount == 0);

        // fill x1

        foreach (immutable key; 0 .. n)
        {
            static if (X.hasValue)
            {
                const value = V.init;
                const element = X.ElementType(key, value);
            }
            else
            {
                const element = key;
            }

            assert(key !in x1);

            assert(x1.length == key);
            assert(x1.insert(element) == InsertionStatus.added);

            static if (X.hasValue)
            {
                const e2 = X.ElementType(key, "a");
                assert(x1.insert(e2) == InsertionStatus.modified);
                assert(x1.contains(e2));
                assert(x1.get(key, null) == "a");
                x1.remove(key);
                x1[key] = value;
            }

            assert(x1.length == key + 1);

            assert(key in x1);
            static if (X.hasValue)
            {
                assert(!x1.contains(X.ElementType(key, "_"))); // other value
            }

            assert(x1.insert(element) == InsertionStatus.unchanged);
            assert(x1.length == key + 1);

            assert(key in x1);
        }

        static if (X.hasValue)
        {
            import basic_uncopyable_array : Array = UncopyableArray;
            Array!(X.ElementType) a1;
            foreach (key; x1.byKey)
            {
                auto eRef = key in x1;
                assert(eRef);
                a1 ~= X.ElementType(key, (*eRef).value);
            }
            assert(x1.length == a1.length);
            foreach (element; a1[])
            {
                auto eRef = element.key in x1;
                assert(eRef);
                assert((*eRef).value == element.value);
            }
        }

        assert(x1.length == n);

        // duplicate x1

        auto x2 = x1.dup;
        assert(x1 == x2);
        assert(x1.bucketCounts.largeCount ==
               x2.bucketCounts.largeCount);
        static assert(!__traits(compiles, { const _ = x1 < x2; })); // no ordering
        assert(x2.length == n);

        // empty x1

        foreach (immutable key; 0 .. n)
        {
            static if (X.hasValue)
            {
                const element = X.ElementType(key, V.init);
            }
            else
            {
                const element = key;
            }

            assert(x1.length == n - key);

            auto hit = key in x1;
            assert(hit);
            static if (X.hasValue)
            {
                assert(*hit == element);
            }

            assert(x1.remove(key));
            assert(x1.length == n - key - 1);

            static if (!X.hasValue)
            {
                assert(!x1.contains(key));
            }
            assert(key !in x1);
            assert(!x1.remove(key));
            assert(x1.length == n - key - 1);
        }

        assert(x1.bucketCounts.largeCount == 0);

        assert(x1.length == 0);

        x1.clear();
        assert(x1.length == 0);

        // empty x2

        assert(x2.length == n); // should be not affected by emptying of x1

        foreach (immutable key; 0 .. n)
        {
            static if (X.hasValue)
            {
                const element = X.ElementType(key, V.init);
            }
            else
            {
                const element = key;
            }

            assert(x2.length == n - key);

            assert(key in x2);

            assert(x2.remove(key));
            assert(x2.length == n - key - 1);

            assert(key !in x2);
            assert(!x2.remove(key));
            assert(x2.length == n - key - 1);
        }

        assert(x2.bucketCounts.largeCount == 0);

        assert(x2.length == 0);

        x2.clear();
        assert(x2.length == 0);
    }
}

/// range checking
pure unittest
{
    import digestx.fnv : FNV;

    immutable n = 11;

    alias K = string;
    alias V = string;

    import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError;

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
    auto s = X.withCapacity(n);

    static if (X.hasValue)
    {
        assertThrown!RangeError(s[K.init]);
        s[K.init] = V.init;
        assertNotThrown!RangeError(s[K.init]);
    }

    s[K.init] = V.init;
    auto vp = K.init in s;
    assert((*vp).value == V.init);

    s.remove(K.init);
    assert(K.init !in s);
}

// version(unittest)
// {
//     private static struct US
//     {
//         @disable this(this);
//         int x;
//     }
// }

// /// uncopyable element type
// pure unittest
// {
//     import digestx.fnv : FNV;

//     immutable n = 11;

//     alias K = US;
//     alias V = string;

//     import std.exception : assertThrown, assertNotThrown;
//     import core.exception : RangeError;

//     alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
//     auto s = X.withCapacity(n);

//     static if (X.hasValue)
//     {
//         assertThrown!RangeError(s[0]);
//         s[0] = V.init;
//         assertNotThrown!RangeError(s[0]);
//     }
// }

version = show;

version(unittest)
{
    import std.meta : AliasSeq;
    import array_help : s;
}

version(show)
{
    import dbgio;
}
