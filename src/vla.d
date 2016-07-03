/** Managing of Variable Length Aggregates (Structs).
 */
module vla;

extern(C) pure nothrow @system @nogc
{
    void* malloc(size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}

/** Construct an instance of a variable-length aggregate (`struct`) type `T`.
    Construction is done using `malloc` plus `emplace`.
    See also: https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
*/
T* constructVariableLength(T, Args...)(size_t requiredCapacity, Args args) @trusted
    if (is(T == struct) ||
        is(T == class))
{
    import std.math : nextPow2;
    import std.algorithm : clamp;
    import std.conv : emplace;
    const paddedRequestedCapacity = (requiredCapacity == 1 ?
                                     1 :
                                     (nextPow2(requiredCapacity - 1).clamp(T.minCapacity,
                                                                           T.maxCapacity)));
    assert(paddedRequestedCapacity >= requiredCapacity);
    return emplace(cast(typeof(return))malloc(T.allocationSizeOfCapacity(paddedRequestedCapacity)),
                   paddedRequestedCapacity, args);
}

/** Check if type `T` is a variable-length aggregate (`struct`) type.
    See also: https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
*/
template hasVariableLength(T)
{
    import std.traits: hasMember;
    enum hasVariableLength = hasMember!(T, "allocationSizeOfCapacity");
}
