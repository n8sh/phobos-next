/** Variable-Length Aggregates (Structs).
    See_Also: https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
 */
module vla;

/** Construct an instance of a variable-length aggregate (`struct`) type `T`.
    Construction is done using `malloc` plus `emplace`.
*/
T* constructVariableLength(T, Args...)(size_t requiredCapacity, Args args) @trusted
    if (is(T == struct) ||
        is(T == class))
{
    import std.math : nextPow2;
    import std.algorithm : clamp;
    const paddedRequestedCapacity = (requiredCapacity == 1 ?
                                     1 :
                                     (nextPow2(requiredCapacity - 1).clamp(T.minCapacity,
                                                                           T.maxCapacity)));
    assert(paddedRequestedCapacity >= requiredCapacity);
    import core.memory : malloc = pureMalloc;
    import std.conv : emplace;
    return emplace(cast(typeof(return))malloc(T.allocationSizeOfCapacity(paddedRequestedCapacity)),
                   paddedRequestedCapacity, args);
}

T* expandVariableLength(T, Args...)(T* curr, size_t requiredCapacity, Args args) @trusted
{
    T* next;
    import qcmeman : free;
    free(cast(voidf*)(&this));
    return next;
}

/** Check if type `T` is a variable-length aggregate (`struct`) type.
*/
template hasVariableLength(T)
{
    import std.traits: hasMember;
    enum hasVariableLength = hasMember!(T, "allocationSizeOfCapacity");
}
