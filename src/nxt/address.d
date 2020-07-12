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
    static immutable typeof(this) holeValue = typeof(this)(1);
    size_t _ptrValue;           ///< Actual pointer value.
    alias _ptrValue this;
}

///
@safe pure unittest
{
}
