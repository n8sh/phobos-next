module sso_open_hashset;

import open_hashmap_or_hashset;

import traits_ex : isAddress;
import container_traits : isNullable;
import pure_mallocator : PureMallocator;

struct SSOOpenHashSet(K,
                      alias hasher = hashOf,
                      alias Allocator = PureMallocator.instance)
    if (isNullable!K)
    {
        import std.traits : hasElaborateDestructor;
        import container_traits : defaultNullKeyConstantOf, isNull, nullify;

        alias InsertionStatus = Large.InsertionStatus;

        @safe:

        static typeof(this) withCapacity()(size_t minimumCapacity) // template-lazy
            @trusted
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

        Large.InsertionStatus insert(K key) @trusted
        {
            if (isLarge)
            {
                return large.insert(key);
            }
            else
            {
                assert(0, "Use static foreach or find");
            }
        }

    private:
        enum borrowChecked = false; // only works if set is not borrow checked

        bool isLarge() const pure nothrow @trusted @nogc
        {
            return small._capacityDummy > Small.maxCapacity;
        }

        union
        {
            alias Large = OpenHashSet!(K, hasher, Allocator, borrowChecked);
            Large large;
            static struct Small
            {
                size_t _capacityDummy; // must be placed at exactly here
                enum maxCapacity = (large.sizeof - _capacityDummy.sizeof)/K.sizeof;
                static assert(maxCapacity, "Cannot fit a single element in a Small");
                K[maxCapacity] _bins;
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

    alias X2 = SSOOpenHashSet!(K, FNV!(64, true));
    auto x2 = X2.withCapacity(2);
    assert(x2.capacity == 2);
    assert(x2.length == 0);

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
