/** Variable-Length Aggregates (Structs).
    See_Also: https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
 */
module nxt.vla;

/** Allocate (via malloc) and emplace an instance of a variable-length aggregate (`struct`) type `T`.
 *
 * Construction is done using `malloc` plus `emplace`.
*/
T* emplaceMallocedVariableLength(T, Args...)(size_t requiredCapacity, Args args) @trusted
if (is(T == struct))
{
    import std.math : nextPow2;
    import std.algorithm : clamp;
    const paddedRequestedCapacity = (requiredCapacity == 1 ?
                                     1 :
                                     (nextPow2(requiredCapacity - 1).clamp(T.minCapacity,
                                                                           T.maxCapacity)));
    assert(paddedRequestedCapacity >= requiredCapacity);
    import nxt.qcmeman : malloc;
    import core.lifetime : emplace;
    return emplace(cast(typeof(return))malloc(T.allocationSizeOfCapacity(paddedRequestedCapacity)),
                   paddedRequestedCapacity, args);
}

/** Check if type `T` is a variable-length aggregate (`struct`) type.
*/
template hasVariableLength(T)
{
    import std.traits: hasMember;
    enum hasVariableLength = hasMember!(T, "allocationSizeOfCapacity");
}
