module nxt.address;

@safe pure:

/** Address as an unsigned integer.
 *
 * Used as key, value or element instead of a pointer `OpenHashMap` to
 * prevent triggering of `gc_addRange` and `gc_removeRange`.
 *
 * TODO: is `holeValue` suitably chosen?
 *
 * See_Also: https://en.wikipedia.org/wiki/Data_structure_alignment
 */
struct ByteAlignedAddress(uint byteAlignment_) // TODO: adjust alignment here
{
@safe pure nothrow @nogc:
    enum byteAlignment = byteAlignment_; ///< Alignment in bytes.

    /// Null value.
    static immutable typeof(this) nullValue = typeof(this).init;

    /// Hole/Deletion value. Prevent hole bitmap from being used.
    static immutable typeof(this) holeValue = typeof(this)(_ptrValue.max);

    /** Get hash of `this`, with extra fast computation for the small case.
     */
    @property hash_t toHash() const scope @trusted
    {
        pragma(inline, true);
        debug checkAlignment();
        static if (byteAlignment == 1)
            const hash = _ptrValue; // as is
        else static if (byteAlignment == 2)
            const hash = _ptrValue >> 1;
        else static if (byteAlignment == 4)
            const hash = _ptrValue >> 2;
        else static if (byteAlignment == 8)
            const hash = _ptrValue >> 3;
        else
            static assert(0, "Unsupported byteAlignment");
        // TODO: activate import nxt.hash_functions : lemireHash64;
        import core.internal.hash : hashOf;
        return hashOf(cast(void*)hash); // TODO: is `cast(void*)` preferred here?
    }

    private debug void checkAlignment() const scope
    {
        assert((_ptrValue & (byteAlignment-1)) == 0,
               "Address is not aligned to " ~ byteAlignment.stringof);
    }

    size_t _ptrValue;           ///< Actual pointer value.
    alias _ptrValue this;
}
alias Address1 = ByteAlignedAddress!(1);
alias Address2 = ByteAlignedAddress!(2);
alias Address4 = ByteAlignedAddress!(4);
alias Address8 = ByteAlignedAddress!(8);

alias Address = Address1;

///
@safe pure unittest             // cannot be @nogc when `opIndex` may throw
{
    import nxt.nullable_traits : hasNullValue, isNullable;
    import nxt.open_hashmap : OpenHashMap;

    static assert(hasNullValue!Address);
    static assert(isNullable!Address);

    OpenHashMap!(Address, Address) m;

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
