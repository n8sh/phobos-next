/** Statically allocated arrays with compile-time known lengths. */
module arrayn;

/** Type of index and slice lifetime checking. */
enum Checking
{
    viaScope,     // compile-time checking of slice lifetimes via DIP-1000 scope
    viaBorrowing  // run-time checking of slice lifetimes and borrows (includes iterator invalidation checking)
}

/** Statically allocated `E`-array of fixed pre-allocated length.  Similar to
    Rust's `fixedvec`: https://docs.rs/fixedvec/0.2.3/fixedvec/

    TODO Merge with array_ex.d to enable reuse of push and pop algorithms
*/
struct ArrayN(E, uint capacity, Checking checking)
{
    import std.bitmanip : bitfields;
    import std.traits : isSomeChar, hasElaborateDestructor, isAssignable;
    import qcmeman : gc_addRange, gc_removeRange;

    E[capacity] _store = void;  /// stored elements

    enum maxLength = capacity;  // for public use

    enum borrowChecked = checking == Checking.viaBorrowing;

    static if (borrowChecked)
    {
        /// Number of bits needed to store number of read borrows.
        enum readBorrowCountBits = 3;
        /// Maximum value possible for `_readBorrowCount`.
        enum readBorrowCountMax = 2^^readBorrowCountBits - 1;

        static      if (capacity <= 2^^(8*ubyte.sizeof - 1 - readBorrowCountBits) - 1)
        {
            mixin(bitfields!(ubyte, "_length", 4, /// number of defined elements in `_store`
                             bool, "_writeBorrowed", 1, // TODO make private
                             uint, "_readBorrowCount", readBorrowCountBits, // TODO make private
                      ));
        }
        else static if (capacity <= 2^^(8*ushort.sizeof - 1 - readBorrowCountBits) - 1)
        {
            mixin(bitfields!(ushort, "_length", 14, /// number of defined elements in `_store`
                             bool, "_writeBorrowed", 1, // TODO make private
                             uint, "_readBorrowCount", readBorrowCountBits, // TODO make private
                      ));
        }
        else static assert("Too large capacity " ~ capacity);
    }
    else
    {
        static if (capacity <= 2^^(8*ubyte.sizeof) - 1)
        {
            ubyte _length;       /// number of defined elements in `_store`
        }
        else static if (capacity <= 2^^(8*ushort.sizeof) - 1)
        {
            ushort _length;       /// number of defined elements in `_store`
        }
        else static assert("Too large capacity " ~ capacity);
    }

    alias ElementType = E;

    /// Is `true` if `U` can be assign to the element type `E` of `this`.
    enum isElementAssignable(U) = isAssignable!(E, U);

    template shouldAddGCRange(T)
    {
        import std.traits : hasIndirections;
        enum shouldAddGCRange = hasIndirections!T; // TODO and only if T's indirections are handled by the GC (not tagged with @nogc)
    }

    @safe pure nothrow @nogc:

    /** Construct with elements `es`. */
    version(none)               // TODO needed?
    {
        pragma(inline) this(Es...)(Es es) @trusted
        if (Es.length >= 1 &&
            Es.length <= capacity && // TODO check conversions `dchar` etc
            allSatisfy!(isElementAssignable, Es))
        {
            static if (shouldAddGCRange!E)
            {
                gc_addRange(_store.ptr, capacity * E.sizeof);
            }
            foreach (const i, ref e; es)
            {
                import std.algorithm.mutation : moveEmplace;
                moveEmplace(e, _store[i]);
            }
            _length = es.length;
        }
    }

    /** Construct with elements in `es`. */
    this(const(E)[] es) @trusted
    {
        assert(es.length <= capacity);
        static if (shouldAddGCRange!E)
        {
            gc_addRange(_store.ptr, capacity * E.sizeof);
        }
        _store[0 .. es.length] = es; // copy
        _length = cast(ubyte)es.length;
    }

    /** Destruct. */
    ~this() nothrow @trusted
    {
        static if (borrowChecked) assert(!isBorrowed);
        static if (hasElaborateDestructor!E)
        {
            foreach (immutable i; 0 .. length)
            {
                .destroy(_store.ptr[i]);
            }
        }
        static if (shouldAddGCRange!E)
        {
            gc_removeRange(_store.ptr);
        }
    }

    /** Returns: `true` if `needle` is contained in `this` haystack. */
    bool canFind(N)(const(N)[] needle) const @trusted // can be trusted
        if (is(typeof(E.init == N.init)))
    {
        import std.algorithm.searching : canFind;
        return _store.ptr[0 .. _length].canFind(needle);
    }

    /** Push/Add elements `es` at back.
        NOTE doesn't invalidate any borrow
     */
    auto ref pushBack(Es...)(Es es) @trusted
        if (Es.length <= capacity)
    {
        assert(_length + Es.length <= capacity);
        foreach (const i, ref e; es)
        {
            import std.algorithm.mutation : moveEmplace;
            moveEmplace(e, _store[_length + i]); // TODO remove `move` when compiler does it for us
        }
        _length = cast(typeof(_length))(_length + Es.length); // TODO better?
        return this;
    }
    /// ditto
    alias append = pushBack;

    import std.traits : isMutable;
    static if (isMutable!E)
    {
        /** Pop first (front) element. */
        auto ref popFront()
        {
            assert(!empty);
            static if (borrowChecked) assert(!isBorrowed);
            // TODO is there a reusable Phobos function for this?
            foreach (const i; 0 .. _length - 1)
            {
                import std.algorithm.mutation : move;
                move(_store[i + 1], _store[i]); // like `_store[i] = _store[i + 1];` but more generic
            }
            _length = cast(typeof(_length))(_length - 1); // TODO better?
            return this;
        }
    }

    /** Pop last (back) element. */
    auto ref popBack()
    {
        assert(!empty);
        static if (borrowChecked) assert(!isBorrowed);
        _length = cast(typeof(_length))(_length - 1); // TODO better?
        static if (hasElaborateDestructor!E)
        {
            .destroy(_store.ptr[_length]);
        }
        return this;
    }

pragma(inline):

    /** Index operator. */
    ref inout(E) opIndex(size_t i) inout @trusted return scope
    {
        assert(i < _length);
        return _store.ptr[i];
    }

    /** First (front) element. */
    ref inout(E) front() inout @trusted return scope
    {
        assert(!empty);
        return _store.ptr[0];
    }

    /** Last (back) element. */
    ref inout(E) back() inout @trusted return scope
    {
        assert(!empty);
        return _store.ptr[_length - 1];
    }

    static if (borrowChecked)
    {
        import borrowed : ReadBorrowed, WriteBorrowed;

        /// Get read-only slice in range `i` .. `j`.
        auto opSlice(size_t i, size_t j) const { return sliceRO(i, j); }
        /// Get read-write slice in range `i` .. `j`.
        auto opSlice(size_t i, size_t j) { return sliceRW(i, j); }

        /// Get read-only full slice.
        auto opSlice() const { return sliceRO(); }
        /// Get read-write full slice.
        auto opSlice() { return sliceRW(); }

        /// Get full read-only slice.
        ReadBorrowed!(E[], typeof(this)) sliceRO() const @trusted
        {
            import std.typecons : Unqual;
            assert(!_writeBorrowed, "typeof(this) is already write-borrowed");
            return typeof(return)(_store.ptr[0 .. _length],
                                  cast(Unqual!(typeof(this))*)(&this)); // trusted unconst casta
        }

        /// Get read-only slice in range `i` .. `j`.
        ReadBorrowed!(E[], typeof(this)) sliceRO(size_t i, size_t j) const @trusted
        {
            import std.typecons : Unqual;
            assert(!_writeBorrowed, "typeof(this) is already write-borrowed");
            return typeof(return)(_store.ptr[i .. j],
                                  cast(Unqual!(typeof(this))*)(&this)); // trusted unconst cast
        }

        /// Get full read-write slice.
        WriteBorrowed!(E[], typeof(this)) sliceRW() @trusted
        {
            assert(!_writeBorrowed, "typeof(this) is already write-borrowed");
            assert(_readBorrowCount == 0, "typeof(this) is already read-borrowed");
            return typeof(return)(_store.ptr[0 .. _length], &this);
        }

        /// Get read-write slice in range `i` .. `j`.
        WriteBorrowed!(E[], typeof(this)) sliceRW(size_t i, size_t j) @trusted
        {
            assert(!_writeBorrowed, "typeof(this) is already write-borrowed");
            assert(_readBorrowCount == 0, "typeof(this) is already read-borrowed");
            return typeof(return)(_store.ptr[0 .. j], &this);
        }

        @safe pure nothrow @nogc @property
        {
            /// Returns: `true` iff `this` is either write or read borrowed.
            bool isBorrowed() const { return _writeBorrowed || _readBorrowCount >= 1; }

            /// Returns: `true` iff `this` is write borrowed.
            bool isWriteBorrowed() const { return _writeBorrowed; }

            /// Returns: number of read-only borrowers of `this`.
            uint readBorrowCount() const { return _readBorrowCount; }
        }
    }
    else
    {
        /// Get slice in range `i` .. `j`.
        inout(E)[] opSlice(size_t i, size_t j) @trusted inout return scope
        {
            assert(i <= j);
            assert(j <= _length);
            return _store.ptr[i .. j];
        }

        /// Get full slice.
        inout(E)[] opSlice() @trusted inout return scope
        {
            return _store.ptr[0 .. _length];
        }
    }

    @safe pure nothrow @nogc @property
    {
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
            const(E)[] toString() const return scope
            {
                return opSlice();
            }
        }
    }
}

/** Stack-allocated string of maximum length of `capacity.` */
alias StringN(uint capacity, Checking checking) = ArrayN!(immutable(char), capacity, checking);
/** Stack-allocated wstring of maximum length of `capacity.` */
alias WStringN(uint capacity, Checking checking) = ArrayN!(immutable(wchar), capacity, checking);
/** Stack-allocated dstring of maximum length of `capacity.` */
alias DStringN(uint capacity, Checking checking) = ArrayN!(immutable(dchar), capacity, checking);

/** Stack-allocated mutable string of maximum length of `capacity.` */
alias MutableStringN(uint capacity, Checking checking) = ArrayN!(char, capacity, checking);
/** Stack-allocated mutable wstring of maximum length of `capacity.` */
alias MutableWStringN(uint capacity, Checking checking) = ArrayN!(char, capacity, checking);
/** Stack-allocated mutable dstring of maximum length of `capacity.` */
alias MutableDStringN(uint capacity, Checking checking) = ArrayN!(char, capacity, checking);

version(unittest)
{
    import std.algorithm.comparison : equal;
    import std.typecons : AliasSeq;
    import std.exception : assertThrown;
    import core.exception : AssertError;
}

/// scope checked string
@safe pure nothrow @nogc unittest
{
    enum capacity = 15;
    foreach (StrN; AliasSeq!(StringN, WStringN, DStringN))
    {
        alias String15 = StrN!(capacity, Checking.viaScope);

        auto x = String15("alphas");

        assert(x[0] == 'a');
        assert(x[$ - 1] == 's');

        assert(x[0 .. 2] == "al");
        assert(x[] == "alphas");

        static if (is(StrN == StringN)) // only test `string` for now
        {
            assert(x.canFind("alpha"));
            assert(x.canFind("al"));
            assert(x.canFind("ph"));
            assert(!x.canFind("ala"));
        }

        const y = String15("åäö_åäöå"); // fits in 15 chars
    }
}

/// scope checked string
version(none) pure unittest     // TODO activate
{
    enum capacity = 15;
    foreach (StrN; AliasSeq!(StringN, WStringN, DStringN))
    {
        alias String15 = StrN!(capacity, Checking.viaScope);
        assertThrown!AssertError(String15("åäö_åäöå_"));
    }
}

/// scope checked string
@safe pure nothrow @nogc unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScope);
    string f()
    {
        auto x = String15("alphas");
        auto y = x[];           // slice to stack allocated (scoped) string
        return y;               // errors with -dip1000
    }
    f();
}

///
pure unittest                   // TODO @safe
{
    alias E = char;
    enum capacity = 3;

    alias A = ArrayN!(E, capacity, Checking.viaBorrowing);
    static assert(A.sizeof == E.sizeof*capacity + 1);

    auto ab = A("ab");
    assert(!ab.empty);
    assert(ab[0] == 'a');
    assert(ab.front == 'a');
    assert(ab.back == 'b');
    assert(ab.length == 2);
    assert(ab[] == "ab");
    assert(ab[0 .. 1] == "a");
    ab.pushBack('_');
    assert(ab[] == "ab_");
    ab.popBack();
    assert(ab[] == "ab");
    assert(ab.toString == "ab");

    const abc = A("abc");
    assert(!abc.empty);
    assert(abc.front == 'a');
    assert(abc.back == 'c');
    assert(abc.length == 3);
    assert(abc[] == "abc");
    assert(ab[0 .. 2] == "ab");
    assert(abc.full);
    static assert(!__traits(compiles, { const abcd = A('a', 'b', 'c', 'd'); })); // too many elements

    assert(ab[] == "ab");
    ab.popFront();
    assert(ab[] == "b");

    const xy = A("xy");
    assert(!xy.empty);
    assert(xy[0] == 'x');
    assert(xy.front == 'x');
    assert(xy.back == 'y');
    assert(xy.length == 2);
    assert(xy[] == "xy");
    assert(xy[0 .. 1] == "x");

    const xyz = A("xyz");
    assert(!xyz.empty);
    assert(xyz.front == 'x');
    assert(xyz.back == 'z');
    assert(xyz.length == 3);
    assert(xyz[] == "xyz");
    assert(xyz.full);
    static assert(!__traits(compiles, { const xyzw = A('x', 'y', 'z', 'w'); })); // too many elements
}

///
pure unittest                   // TODO @safe
{
    static void testAsSomeString(E)()
    {
        enum capacity = 15;
        alias A = ArrayN!(immutable(E), capacity, Checking.viaBorrowing);
        auto a = A("abc");
        assert(a[] == "abc");
        assert(a[].equal("abc"));

        import std.conv : to;
        const x = "a".to!(E[]);
    }

    foreach (E; AliasSeq!(char, wchar, dchar))
    {
        testAsSomeString!E();
    }
}

///
@safe pure nothrow @nogc unittest
{
    enum capacity = 4;
    import std.traits : hasIndirections;
    static assert(hasIndirections!string);
    alias A = ArrayN!(string, capacity, Checking.viaBorrowing);
}

///
@safe pure nothrow @nogc unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaBorrowing);
    static assert(String15.readBorrowCountMax == 7);

    auto x = String15("alpha");

    assert(x.canFind("alpha"));
    assert(x.canFind("al"));
    assert(x.canFind("ph"));
    assert(!x.canFind("ala"));
}

///
pure unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaBorrowing);

    auto x = String15("alpha");

    assert(x[].equal("alpha") &&
           x[].equal("alpha"));

    {
        auto xw1 = x[];
        assert(x.isWriteBorrowed);
        assert(x.isBorrowed);
    }

    auto xr1 = (cast(const)x)[];
    assert(x.readBorrowCount == 1);

    auto xr2 = (cast(const)x)[];
    assert(x.readBorrowCount == 2);

    auto xr3 = (cast(const)x)[];
    assert(x.readBorrowCount == 3);

    auto xr4 = (cast(const)x)[];
    assert(x.readBorrowCount == 4);

    auto xr5 = (cast(const)x)[];
    assert(x.readBorrowCount == 5);

    auto xr6 = (cast(const)x)[];
    assert(x.readBorrowCount == 6);

    auto xr7 = (cast(const)x)[];
    assert(x.readBorrowCount == 7);

    assertThrown!AssertError((cast(const)x)[]);
}

///
@safe pure nothrow @nogc unittest
{
    enum capacity = 4;
    // TODO alias A = ArrayN!(int*, capacity, Checking.viaBorrowing);
}
