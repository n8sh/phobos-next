/** Statically allocated arrays with compile-time known lengths.

    TODO Make scope-checking kick in: http://forum.dlang.org/post/hnrungqxapjwqpkluvqu@forum.dlang.org
 */
module arrayn;

/** Type of index and slice lifetime checking. */
enum Checking
{
    viaScope,     // compile-time checking of slice lifetimes (via DIP-1000 `scope`)
    viaScopeAndBorrowing // run-time checking of slice lifetimes and borrows (includes iterator invalidation checking)
}

/** Statically allocated `E`-array of fixed pre-allocated length.  Similar to
    Rust's `fixedvec`: https://docs.rs/fixedvec/0.2.3/fixedvec/

    TODO Merge with array_ex.d to enable reuse of push and pop algorithms
*/
struct ArrayN(E,
              uint capacity,
              Checking checking = Checking.viaScope)
{
    import std.bitmanip : bitfields;
    import std.typecons : Unqual;
    import std.traits : isSomeChar, hasElaborateDestructor, isAssignable;
    import qcmeman : gc_addRange, gc_removeRange;

     /// stored elements
    debug { E[capacity] _store; }
    else { E[capacity] _store = void; }

    alias This = typeof(this);

    alias maxLength = capacity;  // for public use
    private enum borrowChecked = checking == Checking.viaScopeAndBorrowing;

    static if (borrowChecked)
    {
        /// Number of bits needed to store number of read borrows.
        private enum readBorrowCountBits = 3;

        /// Maximum value possible for `_readBorrowCount`.
        enum readBorrowCountMax = 2^^readBorrowCountBits - 1;

        static      if (capacity <= 2^^(8*ubyte.sizeof - 1 - readBorrowCountBits) - 1)
        {
            private enum lengthMax = 2^^4 - 1;
            mixin(bitfields!(ubyte, "_length", 4, /// number of defined elements in `_store`
                             bool, "_writeBorrowed", 1, // TODO make private
                             uint, "_readBorrowCount", readBorrowCountBits, // TODO make private
                      ));
        }
        else static if (capacity <= 2^^(8*ushort.sizeof - 1 - readBorrowCountBits) - 1)
        {
            private enum lengthMax = 2^^14 - 1;
            mixin(bitfields!(ushort, "_length", 14, /// number of defined elements in `_store`
                             bool, "_writeBorrowed", 1, // TODO make private
                             uint, "_readBorrowCount", readBorrowCountBits, // TODO make private
                      ));
        }
        else
        {
            static assert("Too large capacity " ~ capacity);
        }
    }
    else
    {
        static if (capacity <= ubyte.max)
        {
            ubyte _length;       /// number of defined elements in `_store`
        }
        else static if (capacity <= ushort.max)
        {
            ushort _length;       /// number of defined elements in `_store`
        }
        else
        {
            static assert("Too large capacity " ~ capacity);
        }
    }

    alias MutableE = Unqual!E;

    /// Is `true` if `U` can be assign to the element type `E` of `this`.
    private enum isElementAssignable(U) = isAssignable!(E, U);

    template shouldAddGCRange(T)
    {
        import std.traits : hasIndirections;
        private enum shouldAddGCRange = hasIndirections!T; // TODO and only if T's indirections are handled by the GC (not tagged with @nogc)
    }

    @safe pure:

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
    this(const(MutableE)[] es) @trusted nothrow @nogc
    {
        assert(es.length <= capacity);
        static if (shouldAddGCRange!E)
        {
            gc_addRange(_store.ptr, capacity * E.sizeof);
        }
        // shallow copy needs to cast away shallow constness for now
        _store[0 .. es.length] = (cast(E*)es.ptr)[0 .. es.length];
        _length = cast(ubyte)es.length;
    }

    /** Destruct. */
    ~this() @trusted nothrow @nogc
    {
        static if (borrowChecked) { assert(!isBorrowed); }
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
    bool canFind(N)(const(N)[] needle) const @trusted
        if (is(typeof(E.init == N.init)))
    {
        import std.algorithm.searching : canFind;
        return _store.ptr[0 .. _length].canFind(needle);
    }

    /** Add elements `es` to the back.
        Throws when array becomes full.
        NOTE doesn't invalidate any borrow
     */
    void insertBack(Es...)(Es es) @trusted
        if (Es.length <= capacity) // TODO use `isAssignable`
    {
        import std.exception : enforce;
        enforce(_length + Es.length <= capacity, `Arguments don't fit in array`);
        foreach (const i, ref e; es)
        {
            import std.algorithm.mutation : moveEmplace;
            moveEmplace(e, _store[_length + i]); // TODO remove `move` when compiler does it for us
        }
        _length = cast(typeof(_length))(_length + Es.length); // TODO better?
    }
    /// ditto
    alias put = insertBack;       // `OutputRange` support

    /** Try to add elements `es` to the back.
        NOTE doesn't invalidate any borrow
        Returns: `true` iff all `es` were pushed, `false` otherwise.
     */
    bool insertBackMaybe(Es...)(Es es) @trusted
        if (Es.length <= capacity)
    {
        if (_length + Es.length > capacity) { return false; }
        foreach (const i, ref e; es)
        {
            import std.algorithm.mutation : moveEmplace;
            moveEmplace(e, _store[_length + i]); // TODO remove `move` when compiler does it for us
        }
        _length = cast(typeof(_length))(_length + Es.length); // TODO better?
        return true;
    }
    /// ditto
    alias putMaybe = insertBackMaybe;

    /** Add elements `es` to the back.
        NOTE doesn't invalidate any borrow
     */
    void opOpAssign(string op, Us...)(Us values)
        if (op == "~" &&
            values.length >= 1 &&
            allSatisfy!(isElementAssignable, Us))
    {
        insertBack(move(values)); // TODO remove `move` when compiler does it for
    }

    import std.traits : isMutable;
    static if (isMutable!E)
    {
        /** Pop first (front) element. */
        auto ref popFront() nothrow @nogc
        {
            assert(!empty);
            static if (borrowChecked) { assert(!isBorrowed); }
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
    void popBack() nothrow @nogc
    {
        assert(!empty);
        static if (borrowChecked) { assert(!isBorrowed); }
        _length = cast(typeof(_length))(_length - 1); // TODO better?
        static if (hasElaborateDestructor!E)
        {
            .destroy(_store.ptr[_length]);
        }
    }

    /** Pop the `n` last (back) elements. */
    void popBackN(size_t n) nothrow @nogc
    {
        assert(length >= n);
        static if (borrowChecked) { assert(!isBorrowed); }
        _length = cast(typeof(_length))(_length - n); // TODO better?
        static if (hasElaborateDestructor!E)
        {
            foreach (i; 0 .. n)
            {
                .destroy(_store.ptr[_length + i]);
            }
        }
    }

pragma(inline, true) nothrow @nogc:

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
        auto opSlice(size_t i, size_t j) const return scope { return sliceRO(i, j); }
        /// Get read-write slice in range `i` .. `j`.
        auto opSlice(size_t i, size_t j) return scope { return sliceRW(i, j); }

        /// Get read-only full slice.
        auto opSlice() const return scope { return sliceRO(); }
        /// Get read-write full slice.
        auto opSlice() return scope { return sliceRW(); }

        /// Get full read-only slice.
        ReadBorrowed!(E[], This) sliceRO() const @trusted return scope
        {
            import std.typecons : Unqual;
            assert(!_writeBorrowed, "Already write-borrowed");
            return typeof(return)(_store.ptr[0 .. _length],
                                  cast(Unqual!(This)*)(&this)); // trusted unconst casta
        }

        /// Get read-only slice in range `i` .. `j`.
        ReadBorrowed!(E[], This) sliceRO(size_t i, size_t j) const @trusted return scope
        {
            import std.typecons : Unqual;
            assert(!_writeBorrowed, "Already write-borrowed");
            return typeof(return)(_store.ptr[i .. j],
                                  cast(Unqual!(This)*)(&this)); // trusted unconst cast
        }

        /// Get full read-write slice.
        WriteBorrowed!(E[], This) sliceRW() @trusted return scope
        {
            assert(!_writeBorrowed, "Already write-borrowed");
            assert(_readBorrowCount == 0, "Already read-borrowed");
            return typeof(return)(_store.ptr[0 .. _length], &this);
        }

        /// Get read-write slice in range `i` .. `j`.
        WriteBorrowed!(E[], This) sliceRW(size_t i, size_t j) @trusted return scope
        {
            assert(!_writeBorrowed, "Already write-borrowed");
            assert(_readBorrowCount == 0, "Already read-borrowed");
            return typeof(return)(_store.ptr[0 .. j], &this);
        }

        @property
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

    @property
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
alias StringN(uint capacity, Checking checking = Checking.viaScope) = ArrayN!(immutable(char), capacity, checking);
/** Stack-allocated wstring of maximum length of `capacity.` */
alias WStringN(uint capacity, Checking checking = Checking.viaScope) = ArrayN!(immutable(wchar), capacity, checking);
/** Stack-allocated dstring of maximum length of `capacity.` */
alias DStringN(uint capacity, Checking checking = Checking.viaScope) = ArrayN!(immutable(dchar), capacity, checking);

/** Stack-allocated mutable string of maximum length of `capacity.` */
alias MutableStringN(uint capacity, Checking checking = Checking.viaScope) = ArrayN!(char, capacity, checking);
/** Stack-allocated mutable wstring of maximum length of `capacity.` */
alias MutableWStringN(uint capacity, Checking checking = Checking.viaScope) = ArrayN!(char, capacity, checking);
/** Stack-allocated mutable dstring of maximum length of `capacity.` */
alias MutableDStringN(uint capacity, Checking checking = Checking.viaScope) = ArrayN!(char, capacity, checking);

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

        typeof(String15.init[0])[] xs;
        auto x = String15("alphas");
        xs = x[];               // TODO should error with -dip1000

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
    string f() @safe pure nothrow @nogc
    {
        auto x = String15("alphas");
        auto y = x[];           // slice to stack allocated (scoped) string
        return y;               // TODO should error with -dip1000
    }
    f();
}

///
@safe pure nothrow unittest
{
    import std.exception : assertNotThrown;

    alias E = char;
    enum capacity = 3;

    alias A = ArrayN!(E, capacity, Checking.viaScopeAndBorrowing);
    static assert(A.sizeof == E.sizeof*capacity + 1);

    import std.range : isOutputRange;
    static assert(isOutputRange!(A, E));

    auto ab = A("ab");
    assert(!ab.empty);
    assert(ab[0] == 'a');
    assert(ab.front == 'a');
    assert(ab.back == 'b');
    assert(ab.length == 2);
    assert(ab[] == "ab");
    assert(ab[0 .. 1] == "a");
    assertNotThrown(ab.insertBack('_'));
    assert(ab[] == "ab_");
    ab.popBack();
    assert(ab[] == "ab");
    assert(ab.toString == "ab");

    ab.popBackN(2);
    assert(ab.empty);
    assertNotThrown(ab.insertBack('a', 'b'));

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
@safe pure unittest
{
    static void testAsSomeString(E)()
    {
        enum capacity = 15;
        alias A = ArrayN!(immutable(E), capacity, Checking.viaScopeAndBorrowing);
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
    alias A = ArrayN!(string, capacity, Checking.viaScopeAndBorrowing);
}

///
@safe pure nothrow @nogc unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScopeAndBorrowing);
    static assert(String15.readBorrowCountMax == 7);

    auto x = String15("alpha");

    assert(x.canFind("alpha"));
    assert(x.canFind("al"));
    assert(x.canFind("ph"));
    assert(!x.canFind("ala"));
}

/// assignment from `const` to `immutable` element type
@safe pure nothrow @nogc unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScopeAndBorrowing);
    const char[4] _ = ['a', 'b', 'c', 'd'];
    auto x = String15(_[]);
    assert(x.length == 4);
    assert(x[] == "abcd");
}

///
pure unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScopeAndBorrowing);

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
    // TODO alias A = ArrayN!(int*, capacity, Checking.viaScopeAndBorrowing);
}
