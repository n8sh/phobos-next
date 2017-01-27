/**
 * Dynamic Array
 * Copyright: © 2014 Economic Modeling Specialists, Intl.
 * Authors: Brian Schott
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 */

module dynamicarray;

import std.experimental.allocator.mallocator;

import std.traits;
enum shouldAddGCRange(T) = isPointer!T || hasIndirections!T || is (T == class);

static assert (shouldAddGCRange!string);
static assert (!shouldAddGCRange!int);

/**
 * Array that is able to grow itself when items are appended to it. Uses
 * reference counting to manage memory and malloc/free/realloc for managing its
 * storage.
 * Params:
 *     T = the array element type
 *     supportGC = true if the container should support holding references to
 *         GC-allocated memory.
 */
struct DynamicArray(T, bool supportGC = true)
{
    this(this) @disable; // disable copy constructor

    ~this()
    {
        if (_arr is null)
            return;
        foreach (ref item; _arr[0 .. l])
            typeid(T).destroy(&item);
        static if (shouldAddGCRange!T)
        {
            import core.memory : GC;
            GC.removeRange(_arr.ptr);
        }
        Mallocator.it.deallocate(_arr);
    }

    T opIndex(size_t i) @nogc { return _arr[i]; }
    T[] opSlice() @nogc { return _arr[0 .. l]; }
    T[] opSlice(size_t a, size_t b) @nogc { return _arr[a .. b]; }

    void opIndexAssign(T value, size_t i) @nogc { _arr[i] = value; }
    void opSliceAssign(T value) @nogc { _arr[0 .. l] = value; }
    void opSliceAssign(T value, size_t i, size_t j) @nogc { _arr[i .. j] = value; }

    /** Inserts the given value into the end of the array.
     */
    void insert(T value)
    {
        if (_arr.length == 0)
        {
            _arr = cast(T[]) Mallocator.it.allocate(T.sizeof * 4);
            static if (supportGC && shouldAddGCRange!T)
            {
                import core.memory: GC;
                GC.addRange(_arr.ptr, _arr.length * T.sizeof);
            }
        }
        else if (l >= _arr.length)
        {
            immutable size_t c = _arr.length > 512 ? _arr.length + 1024 : _arr.length << 1;
            void[] a = cast(void[]) _arr;
            Mallocator.it.reallocate(a, c * T.sizeof);
            _arr = cast(T[]) a;
            static if (supportGC && shouldAddGCRange!T)
            {
                import core.memory: GC;
                GC.removeRange(_arr.ptr);
                GC.addRange(_arr.ptr, _arr.length * T.sizeof);
            }
        }
        _arr[l++] = value;
    }
    alias put = insert;
    alias append = insert;

    size_t length() const nothrow pure @property @safe @nogc { return l; }
private:
    T[] _arr; // TODO small size/length optimization (SSO/SLO)
    size_t l;
}

pragma(msg, DynamicArray!int.sizeof);

unittest
{
    import std.algorithm : equal;
    import std.range : iota;
    DynamicArray!int ints;
    foreach (i; 0 .. 100)
        ints.insert(i);
    assert (equal(ints[], iota(100)));
    assert (ints.length == 100);
    ints[0] = 100;
    assert (ints[0] == 100);
    ints[0 .. 5] = 20;
    foreach (i; ints[0 .. 5])
        assert (i == 20);
    ints[] = 432;
    foreach (i; ints[])
        assert (i == 432);
}
