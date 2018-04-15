module sso_open_hashset;

import open_hashmap_or_hashset;

import traits_ex : isAddress;
import container_traits : isNullable;
import pure_mallocator : PureMallocator;

/** Small-set-optimized `OpenHashSet`.
 *
 * TODO use opMove when implemented. See: https://github.com/dlang/DIPs/pull/109
 */
struct SSOOpenHashSet(K,
                      alias hasher = hashOf,
                      alias Allocator = PureMallocator.instance)
if (isNullable!K)
{
    import qcmeman : gc_addRange, gc_removeRange;
    import std.algorithm.mutation : move;
    import std.traits : hasElaborateDestructor;
    import std.conv : emplace;
    import container_traits : defaultNullKeyConstantOf, isNull, nullify, mustAddGCRange;

    alias InsertionStatus = Large.InsertionStatus;

    @safe:

    static typeof(this) withCapacity()(size_t minimumCapacity) @trusted // template-lazy
    {
        typeof(return) result;                      // TODO check zero init
        if (minimumCapacity > Small.maxCapacity)   // small
        {
            // dln("Large init, minimumCapacity:", minimumCapacity);
            result.large = Large.withCapacity(minimumCapacity);
        }
        else
        {
            // dln("Small init, minimumCapacity:", minimumCapacity);
            result.small._capacityDummy = 2;
            static foreach (immutable index; 0 .. small.maxCapacity)
            {
                result.small._bins[index].nullify();
            }
            static if (mustAddGCRange!K)
            {
                gc_addRange(result.small._bins.ptr,
                            result.small._bins.sizeof);
            }
        }
        return result;
    }

    ~this() @trusted
    {
        if (isLarge)
        {
            // dln("Large destroy, capacity:", capacity);
            static if (hasElaborateDestructor!Large)
            {
                .destroy(large);
            }
        }
        else
        {
            // dln("Small destroy, capacity:", capacity);
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

            // try insert in small array
            static foreach (immutable index; 0 .. small.maxCapacity)
            {
                if (small._bins[index].isNull)
                {
                    move(key, small._bins[index]);
                    return InsertionStatus.added;
                }
            }

            // not hit
            expand(1);
            assert(isLarge);
            return large.insert(key);
        }
    }

    private void expand(size_t extraCapacity) @trusted
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
        large.insertN(small._bins[]);
        static if (mustAddGCRange!K)
        {
            gc_removeRange(small._bins.ptr);
        }

        static if (mustAddGCRange!K)
        {
            gc_removeRange(binsCopy.ptr);
        }
    }

private:
    enum borrowChecked = false; // only works if set is not borrow checked

    bool isLarge() const pure nothrow @trusted @nogc
    {
        pragma(inline, true);
        return small._capacityDummy > Small.maxCapacity;
    }

    union
    {
        alias Large = OpenHashSet!(K, hasher, Allocator, borrowChecked);
        Large large;
        static struct Small
        {
            /* must be placed at exactly here (maps to position of
             * `large._bins.length`) and always contain `maxCapacity` when this
             * is small */
            size_t _capacityDummy;
            enum maxCapacity = (large.sizeof - _capacityDummy.sizeof)/K.sizeof;
            static assert(maxCapacity, "Cannot fit a single element in a Small");
            alias Bins = K[maxCapacity];
            Bins _bins;
        }
        Small small;
    };
}

@safe pure unittest
{
    class K
    {
        this(uint value)
        {
            this.value = value;
        }
        uint value;
    }

    // construct small
    alias X2 = SSOOpenHashSet!(K, FNV!(64, true));
    auto x2 = X2.withCapacity(X2.small.maxCapacity);
    assert(!x2.isLarge);
    assert(x2.capacity == X2.small.maxCapacity);
    assert(x2.length == 0);

    // insert first into small
    assert(x2.insert(new K(42)) == x2.InsertionStatus.added);
    assert(!x2.isLarge);
    assert(x2.length == 1);

    // insert second into small
    assert(x2.insert(new K(43)) == x2.InsertionStatus.added);
    assert(!x2.isLarge);
    assert(x2.length == 2);

    // expanding insert third into large
    assert(x2.insert(new K(43)) == x2.InsertionStatus.added);
    assert(x2.isLarge);
    assert(x2.length == 3);

    alias X3 = SSOOpenHashSet!(K, FNV!(64, true));
    auto x3 = X3.withCapacity(3);
    assert(x3.capacity == 4);   // nextPow2
    assert(x3.length == 0);
    assert(x3.insert(new K(42)) == x3.InsertionStatus.added);
    assert(x3.length == 1);
}

version(unittest)
{
    debug import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError, AssertError;
    import std.algorithm : count;
    import std.algorithm.comparison : equal;

    import digestx.fnv : FNV;
    import array_help : s;

    import dbgio;
}
