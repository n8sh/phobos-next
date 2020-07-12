module nxt.address;

@safe pure nothrow @nogc:

/** Address as an unsigned integer.
 *
 * Used as key, value or element instead of a pointer `OpenHashMapOrSet` to
 * prevent triggering of `gc_addRange` and `gc_removeRange`.
 */
struct Address
{
    static immutable typeof(this) nullValue = typeof(this).init;
    static immutable typeof(this) holeValue = typeof(this)(_ptrValue.max); ///< Prevent hole bitmap from being used.
    size_t _ptrValue;           ///< Actual pointer value.
    alias _ptrValue this;
}

///
@safe pure unittest
{
    import nullable_traits : hasNullValue;
    static assert(hasNullValue!Address);

    import nxt.open_hashmap_or_hashset : OpenHashMap;

    alias K = Address;
    alias V = Address;

    OpenHashMap!(K, V) m;

    static assert(m.sizeof == 3*size_t.sizeof); // assure that hole bitmap is not used

    foreach (const address; 1 .. 0x1000)
    {
        const key = address;
        const value = 2*address;

        assert(Address(key) !in m);
        m[Address(key)] = Address(value);
        assert(m[Address(key)] == Address(value));
        assert(Address(key) in m);
    }
}
