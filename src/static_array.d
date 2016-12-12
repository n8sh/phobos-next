module static_array;

/** Statically allocated `E`-array of fixed pre-allocated length.
    Similar to Rust's `fixedvec`: https://docs.rs/fixedvec/0.2.3/fixedvec/

    TODO append/pushBack does not invalidate borrows
    TODO prepend/pushFront
*/
struct ArrayN(E, uint capacity)
{
    import std.bitmanip : bitfields;
    import std.traits : isSomeChar, hasElaborateDestructor;
    import qcmeman : gc_addRange, gc_removeRange;

    E[capacity] _store = void;  /// stored elements

    /// number of elements in `_store`
    static      if (capacity <= 2^^4 - 1)
    {
        /// Maximum value possible for `_readBorrowCount`.
        enum readBorrowCountBits = 3;
        mixin(bitfields!(ubyte, "_length", 4,
                         bool, "_writeBorrowed", 1, // TODO make private
                         uint, "_readBorrowCount", readBorrowCountBits, // TODO make private
                  ));
    }
    else static if (capacity <= 2^^14 - 1)
    {
        enum readBorrowCountBits = 1;
        mixin(bitfields!(ushort, "_length", 14,
                         bool, "_writeBorrowed", 1, // TODO make private
                         uint, "_readBorrowCount", readBorrowCountBits, // TODO make private
                  ));
        /// Maximum value possible for `_readBorrowCount`.
    }
    else static assert("Too large capacity " ~ capacity);

    enum readBorrowCountMax = 2^^readBorrowCountBits - 1;

    alias ElementType = E;

    template shouldAddGCRange(T)
    {
        import std.traits : hasIndirections;
        enum shouldAddGCRange = hasIndirections!T; // TODO and only if T's indirections are handled by the GC (not tagged with @nogc)
    }

    @safe pure nothrow @nogc:

    /** Construct with elements `es`. */
    pragma(inline) this(Es...)(Es es) @trusted
        if (Es.length >= 1 &&
            Es.length <= capacity)
    {
        foreach (const i, const ix; es)
        {
            import std.algorithm.mutation : move;
            _store[i] = ix.move(); // move
        }
        static if (shouldAddGCRange!E)
        {
            gc_addRange(_store.ptr, capacity * E.sizeof);
        }
        _length = es.length;
    }

    /** Construct with elements in `es`. */
    pragma(inline) this(const E[] es) @trusted
    {
        assert(es.length <= capacity);
        _store[0 .. es.length] = es; // copy
        static if (shouldAddGCRange!E)
        {
            gc_addRange(_store.ptr, capacity * E.sizeof);
        }
        _length = cast(ubyte)es.length;
    }

    static if (hasElaborateDestructor!E)
    {
        /** Destruct. */
        pragma(inline) ~this() nothrow @safe
        {
            assert(!isBorrowed);
            destroyElements();
            static if (shouldAddGCRange!E)
            {
                gc_removeRange(_store.ptr);
            }
        }

        /// Destroy elements.
        private void destroyElements() @truste
        {
            static if (hasElaborateDestructor!E)
            {
                foreach (immutable i; 0 .. length)
                {
                    .destroy(_store.ptr[i]);
                }
            }
        }
    }

    /** Returns: `true` if `key` is contained in `this`. */
    bool canFind(const E[] key) const @nogc @trusted
    {
        import std.algorithm.searching : canFind;
        return _store.ptr[0 .. _length].canFind(key);
    }

pragma(inline):

    @safe pure nothrow @nogc @property
    {
        /// Returns: `true` iff `this` is either write or read borrowed.
        bool isBorrowed() const { return _writeBorrowed || _readBorrowCount >= 1; }

        /// Returns: `true` iff `this` is write borrowed.
        bool isWriteBorrowed() const { return _writeBorrowed; }

        /// Returns: number of read-only borrowers of `this`.
        uint readBorrowCount() const { return _readBorrowCount; }

        /** Returns: `true` if `this` is empty, `false` otherwise. */
        bool empty() const { return _length == 0; }

        /** Returns: `true` if `this` is full, `false` otherwise. */
        bool full() const { return _length == capacity; }

        /** Get length. */
        auto length() const { return _length; }
        alias opDollar = length;    /// ditto

        static if (isSomeChar!E)
        {
            /** Get as `string`. */
            const(E)[] toString() const @system // TODO DIP-1000 scope
            {
                return opSlice();
            }
        }
    }

inout:

    /** Index operator. */
    ref inout(E) opIndex(size_t i) @trusted // TODO DIP-1000 scope
    {
        assert(i < _length);
        return _store.ptr[i];
    }

    /** First (front) element. */
    ref inout(E) front() @trusted // TODO DIP-1000 scope
    {
        assert(!empty);
        return _store.ptr[0];
    }

    /** Last (back) element. */
    ref inout(E) back() @trusted // TODO DIP-1000 scope
    {
        assert(!empty);
        return _store.ptr[_length - 1];
    }

    /** Slice operator. */
    inout(E)[] opSlice() @system    // TODO DIP-1000 scope
    {
        return opSlice(0, _length);
    }
    /** ditto */
    inout(E)[] opSlice(size_t i, size_t j) @system // TODO DIP-1000 scope
    {
        assert(i <= j);
        assert(j <= _length);
        return _store.ptr[i .. j];
    }
}

/** Stack-allocated string of maximum length of `capacity.` */
alias StringN(uint capacity) = ArrayN!(immutable(char), capacity);
/** Stack-allocated wstring of maximum length of `capacity.` */
alias WStringN(uint capacity) = ArrayN!(immutable(wchar), capacity);
/** Stack-allocated dstring of maximum length of `capacity.` */
alias DStringN(uint capacity) = ArrayN!(immutable(dchar), capacity);

/** Stack-allocated mutable string of maximum length of `capacity.` */
alias MutableStringN(uint capacity) = ArrayN!(char, capacity);
/** Stack-allocated mutable wstring of maximum length of `capacity.` */
alias MutableWStringN(uint capacity) = ArrayN!(char, capacity);
/** Stack-allocated mutable dstring of maximum length of `capacity.` */
alias MutableDStringN(uint capacity) = ArrayN!(char, capacity);

version(unittest)
{
    import std.algorithm.comparison : equal;
}

///
pure unittest
{
    alias E = char;
    enum capacity = 3;

    alias A = ArrayN!(E, capacity);
    static assert(A.sizeof == E.sizeof*capacity + 1);

    auto ab = A('a', 'b');
    assert(!ab.empty);
    assert(ab[0] == 'a');
    assert(ab.front == 'a');
    assert(ab.back == 'b');
    assert(ab.length == 2);
    assert(ab[] == "ab");
    assert(ab[0 .. 1] == "a");

    const abc = A('a', 'b', 'c');
    assert(!abc.empty);
    assert(abc.front == 'a');
    assert(abc.back == 'c');
    assert(abc.length == 3);
    assert(abc[] == "abc");
    assert(ab[0 .. 2] == "ab");
    assert(abc.full);
    static assert(!__traits(compiles, { const abcd = A('a', 'b', 'c', 'd'); }));

    const xy = A("xy");
    assert(!xy.empty);
    assert(xy[0] == 'x');
    assert(xy.front == 'x');
    assert(xy.back == 'y');
    assert(xy.length == 2);
    assert(xy[] == "xy");
    assert(xy[0 .. 1] == "x");

    const xyz = A('x', 'y', 'z');
    assert(!xyz.empty);
    assert(xyz.front == 'x');
    assert(xyz.back == 'z');
    assert(xyz.length == 3);
    assert(xyz[] == "xyz");
    assert(ab[0 .. 2] == "ab");
    assert(xyz.full);
    static assert(!__traits(compiles, { const xyzw = A('x', 'y', 'z', 'w'); }));
}

///
pure unittest
{
    static void testAsSomeString(E)()
    {
        enum capacity = 15;
        alias A = ArrayN!(immutable(E), capacity);
        auto a = A("abc");
        assert(a[] == "abc");
        assert(a[].equal("abc"));

        import std.conv : to;
        const x = "a".to!(E[]);
    }

    import std.typecons : AliasSeq;
    foreach (E; AliasSeq!(char, wchar, dchar))
    {
        testAsSomeString!E();
    }
}

///
@safe pure nothrow @nogc unittest
{
    enum capacity = 4;
    alias A = ArrayN!(string, capacity);
}

///
@safe pure nothrow @nogc unittest
{
    enum capacity = 15;
    alias String15 = StringN!capacity;
    static assert(String15.readBorrowCountMax == 7);
    auto x = String15("alpha");
    assert(x.canFind("alpha"));
    assert(x.canFind("al"));
    assert(x.canFind("ph"));
    assert(!x.canFind("ala"));
    static assert(is(typeof(x[]) == string));
}
