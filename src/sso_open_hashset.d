module sso_open_hashset;

import open_hashmap_or_hashset;

import std.traits : isInstanceOf;
import traits_ex : isAddress;
import container_traits : isNullable;
import pure_mallocator : PureMallocator;

/** Small-set-optimized `OpenHashSet`.
 *
 * TODO search for `nullify`, `isNull`, `nullValue` and support deleted keys (`isDull`)
 *
 * TODO use opMove to update `gc_addRange` and `gc_removeRange` when
 * implemented. See: https://github.com/dlang/DIPs/pull/109
 */
struct SSOOpenHashSet(K,
                      alias hasher = hashOf,
                      alias Allocator = PureMallocator.instance)
    if (isNullable!K)
{
    import qcmeman : gc_addRange, gc_removeRange;
    import std.algorithm.mutation : move;
    import std.traits : hasElaborateDestructor, isDynamicArray;
    import std.conv : emplace;
    import container_traits : defaultNullKeyConstantOf, isNull, nullify, mustAddGCRange;

    alias InsertionStatus = Large.InsertionStatus;

    @safe:

    static typeof(this) withCapacity()(size_t minimumCapacity) @trusted // template-lazy
    {
        typeof(return) result;                   // TODO `result = void` for nullify case
        if (minimumCapacity > Small.maxCapacity) // will be small
        {
            result.large = Large.withCapacity(minimumCapacity);
        }
        else
        {
            import bit_traits : isAllZeroBits;
            static if (Large.hasAddressLikeKey ||
                       (__traits(hasMember, K, `nullValue`) && // if key has a null value
                        __traits(compiles, { enum _ = isAllZeroBits!(K, K.nullValue); }) && // prevent strange error given when `K` is `knet.data.Data`
                        isAllZeroBits!(K, K.nullValue))) // check that it's zero bits only
            {
                // nothing needed
            }
            else                // needs explicit null
            {
                static foreach (immutable index; 0 .. small.maxCapacity)
                {
                    result.small._bins[index].nullify();
                }
            }

            static if (mustAddGCRange!K)
            {
                gc_addRange(result.small._bins.ptr,
                            result.small._bins.sizeof);
            }

            result.small._capacityDummy = 2; // tag as small
        }
        return result;
    }

    ~this() @trusted
    {
        if (isLarge)
        {
            static if (hasElaborateDestructor!Large)
            {
                .destroy(large);
            }
        }
        else
        {
            static if (hasElaborateDestructor!K)
            {
                static assert(0, "Destroy non-null elements");
            }
        }
    }

    @disable this(this);

    @property size_t capacity() pure nothrow @trusted @nogc
    {
        pragma(inline, true);
        return small._capacityDummy;
    }

    @property size_t length() pure nothrow @trusted @nogc
    {
        version(LDC) pragma(inline, true);
        if (isLarge)
        {
            return large.length;
        }
        else
        {
            import std.algorithm.searching : count;
            return small._bins[].count!(_ => !_.isNull);
        }
    }

    InsertionStatus insert(K key) @trusted
    {
        if (isLarge)
        {
            return large.insert(key);
        }
        else
        {
            assert(!key.isNull);

            // try inserting into small
            static foreach (immutable index; 0 .. small.maxCapacity)
            {
                if (small._bins[index].isNull) // free slot
                {
                    move(key, small._bins[index]);
                    return InsertionStatus.added;
                }
            }

            // not hit
            expandWithExtraCapacity(1);
            assert(isLarge);
            return large.insert(key);
        }
    }

    /** Check if `element` is stored.
        Returns: `true` if element is present, `false` otherwise.
    */
    bool contains(const scope K key) const @trusted // template-lazy, `auto ref` here makes things slow
    {
        if (isLarge)
        {
            return large.contains(key);
        }
        else
        {
            assert(!key.isNull);
            // TODO is static foreach faster here?
            import std.algorithm.searching : canFind;
            alias pred = (a, b) => a is b;            // TODO add to template
            return small._bins[].canFind!(pred)(key);
        }
    }

    private void expandWithExtraCapacity(size_t extraCapacity) @trusted
    {
        Small.Bins binsCopy = small._bins;
        static if (mustAddGCRange!K)
        {
            gc_addRange(binsCopy.ptr,
                        binsCopy.sizeof);
        }

        // TODO merge these lines?
        emplace!Large(&large);
        large.reserveExtra(Small.maxCapacity + extraCapacity);
        large.insertN(binsCopy);
        static if (mustAddGCRange!K)
        {
            gc_removeRange(small._bins.ptr);
        }

        static if (mustAddGCRange!K)
        {
            gc_removeRange(binsCopy.ptr);
        }
    }

    auto byLvalueElement()() const @safe // template-lazy
    {
        import std.algorithm.iteration : filter;
        return bins[].filter!(bin => !bin.isNull);
    }

    private inout(K)[] bins() inout @trusted
    {
        pragma(inline, true);
        if (isLarge)
        {
            return large.rawBins;
        }
        else
        {
            return small._bins[];
        }
    }

private:
    enum borrowChecked = false; // only works if set is not borrow checked

    bool isLarge() const pure nothrow @trusted @nogc
    {
        pragma(inline, true);
        return small._capacityDummy > Small.maxCapacity;
    }

    /// Returns: `true` if `this` currently uses small (packed) array storage.
    bool isSmall() const pure nothrow @trusted @nogc { return !isLarge; }

    union
    {
        alias Large = OpenHashSet!(K, hasher, Allocator, borrowChecked);
        Large large;
        static struct Small
        {
            /* discriminator between large and small storage; must be placed at
             * exactly here (maps to position of `large._bins.length`) and
             * always contain `maxCapacity` when this is small */
            size_t _capacityDummy;
            enum maxCapacity = (large.sizeof - _capacityDummy.sizeof)/K.sizeof;
            static assert(maxCapacity, "Cannot fit a single element in a Small");
            alias Bins = K[maxCapacity];
            Bins _bins;
        }
        Small small;
    };
}

/** Returns: range that iterates through the elements of `c` in undefined order.
 */
auto byElement(Table)(auto ref return const(Table) c) @trusted
    if (isInstanceOf!(SSOOpenHashSet, Table))
{
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        return c.byLvalueElement();
    }
    else                        // `c` was is an r-value and can be moved
    {
        static assert(0, "R-value Table not supported");
    }
}
alias range = byElement;        // EMSI-container naming

/// start small and expand to large
@safe pure unittest
{
    // construct small
    alias X = SSOOpenHashSet!(K, FNV!(64, true));
    static assert(X.sizeof == 24);
    auto x = X.withCapacity(X.small.maxCapacity);
    assert(x.isSmall);
    assert(x.capacity == X.small.maxCapacity);
    assert(x.length == 0);

    auto k42 = new K(42);
    auto k43 = new K(43);
    auto k44 = new K(44);

    // insert first into small
    assert(!x.contains(k42));
    assert(x.insert(k42) == x.InsertionStatus.added);
    assert(x.contains(k42));
    assert(x.byElement.equal!((a, b) => a is b)([k42].s[]));
    assert(x.isSmall);
    assert(x.length == 1);

    // insert second into small
    assert(!x.contains(k43));
    assert(x.insert(k43) == x.InsertionStatus.added);
    assert(x.contains(k42));
    assert(x.contains(k43));
    assert(x.byElement.equal!((a, b) => a is b)([k42, k43].s[]));
    assert(x.isSmall);
    assert(x.length == 2);

    // expanding insert third into large
    assert(!x.contains(k44));
    assert(x.insert(k44) == x.InsertionStatus.added);
    // unordered store so equal doesn't work anymore
    assert(x.contains(k42));
    assert(x.contains(k43));
    assert(x.contains(k44));
    foreach (ref e; x.byElement)
    {
        assert(x.contains(e));
    }
    assert(x.isLarge);
    assert(x.length == 3);
}

/// start large
@safe pure unittest
{
    alias X = SSOOpenHashSet!(K, FNV!(64, true));
    auto x = X.withCapacity(3);
    assert(x.isLarge);
    assert(x.capacity == 4);    // nextPow2(3)
    assert(x.length == 0);
    assert(x.insert(new K(42)) == x.InsertionStatus.added);
    assert(x.length == 1);
    assert(x.insert(new K(43)) == x.InsertionStatus.added);
    assert(x.length == 2);
}

version(unittest)
{
    class K
    {
        this(uint value) @safe pure nothrow @nogc
        {
            this.value = value;
        }
        uint value;
    }

    import std.algorithm.comparison : equal;
    import digestx.fnv : FNV;
    import array_help : s;

    import dbgio;
}
