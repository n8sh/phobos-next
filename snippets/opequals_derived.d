/** Returns `true` iff `lhs` and `rhs` are equal.
 *
 * Opposite to druntime version, implementation is parameterized on object type
 * `T` enabling correct propagation of function qualifiers of `lhs.opEquals(rhs)`.
 */
bool opEqualsDerived(T)(const T lhs, const T rhs)
    if (is(T == class))
    {
        // If aliased to the same object or both null => equal
        if (lhs is rhs) return true;

        // If either is null => non-equal
        if (lhs is null || rhs is null) return false;

        return lhs.opEquals(rhs);

        version(none)
        {
            // If same exact type => one call to method opEquals
            if (typeid(lhs) is typeid(rhs)//  ||
                // TODO !__ctfe && typeid(lhs).opEquals(typeid(rhs))
                )
                /* CTFE doesn't like typeid much. 'is' works, but opEquals doesn't
                   (issue 7147). But CTFE also guarantees that equal TypeInfos are
                   always identical. So, no opEquals needed during CTFE. */
            {
                return lhs.opEquals(rhs);
            }

            // General case => symmetric calls to method opEquals
            return lhs.opEquals(rhs) && rhs.opEquals(lhs);
        }
    }

///
@safe pure nothrow unittest
{
    class C
    {
        @safe pure nothrow @nogc:
        this(int x)
        {
            this.x = x;
        }
        @property bool opEquals(const scope typeof(this) rhs) const
        {
            return x == rhs.x;
        }
        @property override bool opEquals(const scope Object rhs) const @trusted
        {
            C rhs_ = cast(C)rhs;
            return rhs_ && x == rhs_.x;
        }
        int x;
    }
    assert( opEqualsDerived(new C(42), new C(42)));
    assert(!opEqualsDerived(new C(42), new C(43)));
}
