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

/** Statically allocated `T`-array of fixed pre-allocated length.  Similar to
    Rust's `fixedvec`: https://docs.rs/fixedvec/0.2.3/fixedvec/

    TODO Merge member functions with basic_*_array.d and array_ex.d

    TODO Add @safe nothrow @nogc ctor from static array (of known length)
*/
struct ArrayN(T,
              uint minimumCapacity,
              Checking checking = Checking.viaScope)
{
    import std.bitmanip : bitfields;
    import std.traits : Unqual;
    import std.traits : isSomeChar, hasElaborateDestructor, isAssignable, isCopyable;
    import std.algorithm.mutation : move, moveEmplace;

    import qcmeman : gc_addRange, gc_removeRange;
    import container_traits : mustAddGCRange;

    alias capacity = minimumCapacity; // for public use

    /// Store of `capacity` number of elements.
    T[capacity] _store;         // TODO use store constructor

    /// Is `true` iff `this` has borrow-checked slicing.
    enum borrowChecked = checking == Checking.viaScopeAndBorrowing;

    static if (borrowChecked)
    {
        /// Number of bits needed to store number of read borrows.
        private enum readBorrowCountBits = 3;

        /// Maximum value possible for `_readBorrowCount`.
        enum readBorrowCountMax = 2^^readBorrowCountBits - 1;

        static      if (capacity <= 2^^(8*ubyte.sizeof - 1 - readBorrowCountBits) - 1)
        {
            private enum lengthMax = 2^^4 - 1;
            alias Length = ubyte;
            // TODO make private:
            mixin(bitfields!(Length, "_length", 4, /// number of defined elements in `_store`
                             bool, "_writeBorrowed", 1,
                             uint, "_readBorrowCount", readBorrowCountBits,
                      ));
        }
        else static if (capacity <= 2^^(8*ushort.sizeof - 1 - readBorrowCountBits) - 1)
        {
            alias Length = ushort;
            private enum lengthMax = 2^^14 - 1;
            // TODO make private:
            mixin(bitfields!(Length, "_length", 14, /// number of defined elements in `_store`
                             bool, "_writeBorrowed", 1,
                             uint, "_readBorrowCount", readBorrowCountBits,
                      ));
        }
        else
        {
            static assert("Too large requested capacity " ~ capacity);
        }
    }
    else
    {
        static if (capacity <= ubyte.max)
        {
            alias Length = ubyte;
        }
        else static if (capacity <= ushort.max)
        {
            alias Length = ushort;
        }
        else
        {
            static assert("Too large requested capacity " ~ capacity);
        }
        Length _length;         /// number of defined elements in `_store`
    }

    /// Is `true` iff `U` can be assign to the element type `T` of `this`.
    private enum isElementAssignable(U) = isAssignable!(T, U);

    @safe:

    /// Construct from element `values`.
    this(Us...)(Us values) @trusted
        if (Us.length <= capacity)
    {
        static if (mustAddGCRange!T)
        {
            gc_addRange(_store.ptr, _store.sizeof);
        }

        foreach (const ix, ref value; values)
        {
            static if (hasElaborateDestructor!(typeof(value)))
            {
                // TODO moveEmplace
            }
            _store[ix] = value;
        }
        _length = cast(Length)values.length;
        static if (borrowChecked)
        {
            _writeBorrowed = false;
            _readBorrowCount = 0;
        }
    }

    /// Construct from element `values`.
    this(U)(U[] values) @trusted
        if (isCopyable!U//  &&
            // TODO isElementAssignable!U
            ) // prevent accidental move of l-value `values` in array calls
    {
        import std.exception : enforce;
        enforce(values.length <= capacity, `Arguments don't fit in array`);

        static if (mustAddGCRange!T)
        {
            gc_addRange(_store.ptr, _store.sizeof);
        }

        _store[0 .. values.length] = values;
        _length = cast(Length)values.length;
        static if (borrowChecked)
        {
            _writeBorrowed = false;
            _readBorrowCount = 0;
        }
    }

    /// Construct from element `values`.
    static typeof(this) fromValuesUnsafe(U)(U[] values) @system
        if (isCopyable!U &&
            isElementAssignable!U
            ) // prevent accidental move of l-value `values` in array calls
    {
        typeof(return) that;              // TODO use Store constructor:
        static if (mustAddGCRange!T)
        {
            gc_addRange(that._store.ptr, that._store.sizeof);
        }

        that._store[0 .. values.length] = values;
        that._length = cast(Length)values.length;

        static if (borrowChecked)
        {
            that._writeBorrowed = false;
            that._readBorrowCount = 0;
        }

        return that;
    }

    static if (borrowChecked ||
               hasElaborateDestructor!T ||
               mustAddGCRange!T)
    {
        /** Destruct. */
        ~this() @trusted
        {
            static if (borrowChecked) { assert(!isBorrowed); }
            static if (hasElaborateDestructor!T)
            {
                foreach (const i; 0 .. length)
                {
                    .destroy(_store.ptr[i]);
                }
            }
            static if (mustAddGCRange!T)
            {
                gc_removeRange(_store.ptr);
            }
        }
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
            moveEmplace(e, _store[_length + i]); // TODO remove `move` when compiler does it for us
        }
        _length = cast(Length)(_length + Es.length); // TODO better?
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
            moveEmplace(e, _store[_length + i]); // TODO remove `move` when compiler does it for us
        }
        _length = cast(Length)(_length + Es.length); // TODO better?
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
    static if (isMutable!T)
    {
        /** Pop first (front) element. */
        auto ref popFront()
        {
            assert(!empty);
            static if (borrowChecked) { assert(!isBorrowed); }
            // TODO is there a reusable Phobos function for this?
            foreach (const i; 0 .. _length - 1)
            {
                move(_store[i + 1], _store[i]); // like `_store[i] = _store[i + 1];` but more generic
            }
            _length = cast(typeof(_length))(_length - 1); // TODO better?
            return this;
        }
    }

    /** Pop last (back) element. */
    void popBack()()            // template-lazy
    {
        assert(!empty);
        static if (borrowChecked) { assert(!isBorrowed); }
        _length = cast(Length)(_length - 1); // TODO better?
        static if (hasElaborateDestructor!T)
        {
            .destroy(_store.ptr[_length]);
        }
    }

    /** Pop the `n` last (back) elements. */
    void popBackN()(size_t n)   // template-lazy
    {
        assert(length >= n);
        static if (borrowChecked) { assert(!isBorrowed); }
        _length = cast(Length)(_length - n); // TODO better?
        static if (hasElaborateDestructor!T)
        {
            foreach (i; 0 .. n)
            {
                .destroy(_store.ptr[_length + i]);
            }
        }
    }

    /** Move element at `index` to return. */
    static if (isMutable!T)
    {
        /** Pop element at `index`. */
        void popAt()(size_t index) // template-lazy
        @trusted
        @("complexity", "O(length)")
        {
            assert(index < this.length);
            .destroy(_store.ptr[index]);
            shiftToFrontAt(index);
            _length = cast(Length)(_length - 1);
        }

        T moveAt()(size_t index) // template-lazy
        @trusted
        @("complexity", "O(length)")
        {
            assert(index < this.length);
            auto value = move(_store.ptr[index]);
            shiftToFrontAt(index);
            _length = cast(Length)(_length - 1);
            return move(value); // TODO remove `move` when compiler does it for us
        }

        private void shiftToFrontAt()(size_t index) // template-lazy
            @trusted
        {
            foreach (immutable i; 0 .. this.length - (index + 1))
            {
                immutable si = index + i + 1; // source index
                immutable ti = index + i;     // target index
                moveEmplace(_store.ptr[si],
                            _store.ptr[ti]);
            }
        }
    }

pragma(inline, true):

    /** Index operator. */
    ref inout(T) opIndex(size_t i) inout @trusted return scope
    {
        assert(i < _length);
        return _store.ptr[i];
    }

    /** First (front) element. */
    ref inout(T) front() inout @trusted return scope
    {
        assert(!empty);
        return _store.ptr[0];
    }

    /** Last (back) element. */
    ref inout(T) back() inout @trusted return scope
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
        ReadBorrowed!(T[], typeof(this)) sliceRO() const @trusted return scope
        {
            import std.traits : Unqual;
            assert(!_writeBorrowed, "Already write-borrowed");
            return typeof(return)(_store.ptr[0 .. _length],
                                  cast(Unqual!(typeof(this))*)(&this)); // trusted unconst casta
        }

        /// Get read-only slice in range `i` .. `j`.
        ReadBorrowed!(T[], typeof(this)) sliceRO(size_t i, size_t j) const @trusted return scope
        {
            import std.traits : Unqual;
            assert(!_writeBorrowed, "Already write-borrowed");
            return typeof(return)(_store.ptr[i .. j],
                                  cast(Unqual!(typeof(this))*)(&this)); // trusted unconst cast
        }

        /// Get full read-write slice.
        WriteBorrowed!(T[], typeof(this)) sliceRW() @trusted return scope
        {
            assert(!_writeBorrowed, "Already write-borrowed");
            assert(_readBorrowCount == 0, "Already read-borrowed");
            return typeof(return)(_store.ptr[0 .. _length], &this);
        }

        /// Get read-write slice in range `i` .. `j`.
        WriteBorrowed!(T[], typeof(this)) sliceRW(size_t i, size_t j) @trusted return scope
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
        inout(T)[] opSlice(size_t i, size_t j) @trusted inout return scope
        {
            assert(i <= j);
            assert(j <= _length);
            return _store.ptr[i .. j];
        }

        /// Get full slice.
        inout(T)[] opSlice() @trusted inout return scope
        {
            return _store.ptr[0 .. _length];
        }
    }

    @property
    {
        /** Returns: `true` iff `this` is empty, `false` otherwise. */
        bool empty() const { return _length == 0; }

        /** Returns: `true` iff `this` is full, `false` otherwise. */
        bool full() const { return _length == capacity; }

        /** Get length. */
        auto length() const { return _length; }
        alias opDollar = length;    /// ditto

        static if (isSomeChar!T)
        {
            /** Get as `string`. */
            const(T)[] toString() const return scope
            {
                return opSlice();
            }
        }
    }

    /** Comparison for equality. */
    bool opEquals(in typeof(this) rhs) const
    {
        return this[] == rhs[];
    }
    /// ditto
    bool opEquals(in ref typeof(this) rhs) const
    {
        return this[] == rhs[];
    }
    /// ditto
    bool opEquals(U)(in U[] rhs) const
        if (is(typeof(T[].init == U[].init)))
    {
        return this[] == rhs;
    }
}

/** Stack-allocated string of maximum length of `minimumCapacity.` */
alias StringN(uint minimumCapacity, Checking checking = Checking.viaScope) = ArrayN!(immutable(char), minimumCapacity, checking);
/** Stack-allocated wstring of maximum length of `minimumCapacity.` */
alias WStringN(uint minimumCapacity, Checking checking = Checking.viaScope) = ArrayN!(immutable(wchar), minimumCapacity, checking);
/** Stack-allocated dstring of maximum length of `minimumCapacity.` */
alias DStringN(uint minimumCapacity, Checking checking = Checking.viaScope) = ArrayN!(immutable(dchar), minimumCapacity, checking);

/** Stack-allocated mutable string of maximum length of `minimumCapacity.` */
alias MutableStringN(uint minimumCapacity, Checking checking = Checking.viaScope) = ArrayN!(char, minimumCapacity, checking);
/** Stack-allocated mutable wstring of maximum length of `minimumCapacity.` */
alias MutableWStringN(uint minimumCapacity, Checking checking = Checking.viaScope) = ArrayN!(char, minimumCapacity, checking);
/** Stack-allocated mutable dstring of maximum length of `minimumCapacity.` */
alias MutableDStringN(uint minimumCapacity, Checking checking = Checking.viaScope) = ArrayN!(char, minimumCapacity, checking);

/// construct from array may throw
@safe pure unittest
{
    enum capacity = 3;
    alias T = int;
    alias A = ArrayN!(T, capacity);
    static assert(!mustAddGCRange!A);

    auto a = A([1, 2, 3].s[]);
    assert(a[] == [1, 2, 3].s);
}

/// unsafe construct from array
@trusted pure nothrow @nogc unittest
{
    enum capacity = 3;
    alias T = int;
    alias A = ArrayN!(T, capacity);
    static assert(!mustAddGCRange!A);

    auto a = A.fromValuesUnsafe([1, 2, 3].s);
    assert(a[] == [1, 2, 3].s);
}

/// construct from scalars is nothrow
@safe pure nothrow @nogc unittest
{
    enum capacity = 3;
    alias T = int;
    alias A = ArrayN!(T, capacity);
    static assert(!mustAddGCRange!A);

    auto a = A(1, 2, 3);
    assert(a[] == [1, 2, 3].s);

    static assert(!__traits(compiles, { auto _ = A(1, 2, 3, 4); }));
}

/// scope checked string
@safe pure unittest
{
    enum capacity = 15;
    foreach (StrN; AliasSeq!(StringN// , WStringN, DStringN
                 ))
    {
        alias String15 = StrN!(capacity, Checking.viaScope);

        typeof(String15.init[0])[] xs;
        auto x = String15("alphas");
        xs = x[];               // TODO should error with -dip1000

        assert(x[0] == 'a');
        assert(x[$ - 1] == 's');

        assert(x[0 .. 2] == "al");
        assert(x[] == "alphas");

        const y = String15("åäö_åäöå"); // fits in 15 chars
    }
}

/// scope checked string
version(none) pure unittest     // TODO activate
{
    enum capacity = 15;
    foreach (StrN; AliasSeq!(StringN, WStringN, DStringN))
    {
        static assert(!mustAddGCRange!StrN);
        alias String15 = StrN!(capacity, Checking.viaScope);
        assertThrown!AssertError(String15("åäö_åäöå_"));
    }
}

/// scope checked string
@safe pure unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScope);
    static assert(!mustAddGCRange!String15);

    string f() @safe pure
    {
        auto x = String15("alphas");
        auto y = x[];           // slice to stack allocated (scoped) string
        return y;               // TODO should error with -dip1000
    }
    f();
}

///
@safe pure unittest
{
    import std.exception : assertNotThrown;

    alias T = char;
    enum capacity = 3;

    alias A = ArrayN!(T, capacity, Checking.viaScopeAndBorrowing);
    static assert(!mustAddGCRange!A);
    static assert(A.sizeof == T.sizeof*capacity + 1);

    import std.range : isOutputRange;
    static assert(isOutputRange!(A, T));

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
    static void testAsSomeString(T)()
    {
        enum capacity = 15;
        alias A = ArrayN!(immutable(T), capacity, Checking.viaScopeAndBorrowing);
        static assert(!mustAddGCRange!A);
        auto a = A("abc");
        assert(a[] == "abc");
        assert(a[].equal("abc"));

        import std.conv : to;
        const x = "a".to!(T[]);
    }

    foreach (T; AliasSeq!(char// , wchar, dchar
                 ))
    {
        testAsSomeString!T();
    }
}

///
@safe pure unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScopeAndBorrowing);
    static assert(!mustAddGCRange!String15);
    static assert(String15.readBorrowCountMax == 7);
    auto x = String15("alpha");
}

/// equality
pure unittest
{
    enum capacity = 15;
    alias S = ArrayN!(int, capacity);
    static assert(!mustAddGCRange!S);

    assert(S([1, 2, 3].s[]) ==
           S([1, 2, 3].s[]));
    assert(S([1, 2, 3].s[]) ==
           [1, 2, 3]);
}

/// equality
pure nothrow @nogc unittest
{
    enum capacity = 15;
    alias S = ArrayN!(int, capacity);

    assert(S.fromValuesUnsafe([1, 2, 3].s) ==
           S.fromValuesUnsafe([1, 2, 3].s));

    const ax = [1, 2, 3].s;
    assert(S.fromValuesUnsafe([1, 2, 3].s) == ax);
    assert(S.fromValuesUnsafe([1, 2, 3].s) == ax[]);

    const cx = [1, 2, 3].s;
    assert(S.fromValuesUnsafe([1, 2, 3].s) == cx);
    assert(S.fromValuesUnsafe([1, 2, 3].s) == cx[]);

    immutable ix = [1, 2, 3].s;
    assert(S.fromValuesUnsafe([1, 2, 3].s) == ix);
    assert(S.fromValuesUnsafe([1, 2, 3].s) == ix[]);
}

/// assignment from `const` to `immutable` element type
@safe pure unittest
{
    enum capacity = 15;
    alias String15 = StringN!(capacity, Checking.viaScopeAndBorrowing);
    static assert(!mustAddGCRange!String15);

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
    static assert(!mustAddGCRange!String15);

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

version(unittest)
{
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;
    import std.exception : assertThrown;
    import core.exception : AssertError;

    import array_help : s;
    import container_traits : mustAddGCRange;
}
