/** Ownership and borrwoing á lá Rust.

    TODO Move to typecons_ex.
    TODO Perhaps disable all checking (and unittests) in release mode (when debug is not active)
    TODO Implement and use trait `hasUnsafeSlicing`
 */
module borrown;

version(unittest)
{
    import dbgio;
}

/** Return wrapper around container `Container` that can be safely sliced, by
    tracking number of read borrowed ranges and whether it's currently write
    borrowed.

    Only relevant when `Container` implements referenced access over
    - `opSlice` and
    - `opIndex`

    TODO Iterate and wrap all @unsafe accessors () and wrapped borrow
    checks for all modifying members of `Container`?
*/
struct Owned(Container)
    if (needsOwnership!Container)
{
    import std.range.primitives : hasSlicing;
    import std.traits : isMutable;

pragma(inline):

    /// Type of range of `Container`.
    alias Range = typeof(Container.init[]);

    // TODO can we somehow disallow move construction for `this`?

    ~this()
    {
        assert(!_writeBorrowed, "This is still write-borrowed, cannot release!");
        assert(_readerCount == 0, "This is still read-borrowed, cannot release!");
    }

    /// Move `this` into a return r-value.
    typeof(this) move()
    {
        assert(!_writeBorrowed, "This is still write-borrowed, cannot move!");
        assert(_readerCount == 0, "This is still read-borrowed, cannot move!");
        import std.algorithm.mutation : move;
        return move(this);
    }

    /** Checked overload for move. */
    void move(ref typeof(this) dst) @safe pure nothrow @nogc
    {
        assert(!this._writeBorrowed, "Source is still write-borrowed, cannot move!");
        assert(this._readerCount == 0, "Source is still read-borrowed, cannot move!");

        assert(!dst._writeBorrowed, "Destination is still write-borrowed, cannot move!");
        assert(dst._readerCount == 0, "Destination is still read-borrowed, cannot move!");

        import std.algorithm.mutation : move;
        move(this, dst);
    }

    static if (true/*TODO hasUnsafeSlicing!Container*/)
    {
        /// Get full read-only slice.
        ReadBorrowedSlice!(Range, Owned) readOnlySlice() @trusted // TODO shorter name?
        {
            assert(!_writeBorrowed, "This is already write-borrowed!");
            return typeof(return)(_range.opSlice, &this);
        }

        /// Get read-only slice in range i .. j.
        ReadBorrowedSlice!(Range, Owned) readOnlySlice(size_t i, size_t j) @trusted // TODO shorter name?
        {
            assert(!_writeBorrowed, "This is already write-borrowed!");
            return typeof(return)(_range.opSlice[i .. j], &this);
        }

        alias opSlice = readOnlySlice; // TODO default to read or write?

        static if (isMutable!Container)
        {
            /// Get full read-write slice.
            WriteBorrowedSlice!(Range, Owned) writableSlice() @trusted // TODO shorted name?
            {
                assert(!_writeBorrowed, "This is already write-borrowed!");
                assert(_readerCount == 0, "This is already read-borrowed!");
                return typeof(return)(_range.opSlice, &this);
            }

            /// Get read-write slice in range i .. j.
            WriteBorrowedSlice!(Range, Owned) writableSlice(size_t i, size_t j) @trusted // TODO shorted name?
            {
                assert(!_writeBorrowed, "This is already write-borrowed!");
                assert(_readerCount == 0, "This is already read-borrowed!");
                return typeof(return)(_range.opSlice[i .. j], &this);
            }
        }
    }

    @safe pure nothrow @nogc:

    @property:

    /// Returns: `true` iff currently is write borrowed.
    bool writerBorrowed() const { return _writeBorrowed; }

    /// Returns: number of read-only borrowers.
    uint readerCount() const { return _readerCount; }

private:
    Container _range;            /// wrapped container
    bool _writeBorrowed = false; /// `true' if _range is currently referred to
    uint _readerCount = 0;       /// number of readable borrowers. TODO use `size_t` minus one bit instead in `size_t _stats`
    alias _range this;
}

/** Checked overload for `std.algorithm.mutation.move`.

    TODO Can we somehow prevent users of Owned from accidentally using
    `std.algorithm.mutation.move` instead of this wrapper?
 */
void move(Owner)(ref Owner src, ref Owner dst) @safe pure nothrow @nogc
    if (isInstanceOf!(Owned, Owner))
{
    src.move(dst);              // reuse member function
}

// import std.traits : isInstanceOf;

/** Write-borrowed access to range `Range`. */
private static struct WriteBorrowedSlice(Range, Owner)
    // if (isInstanceOf!(Owned, Owner))
{
    this(Range range, Owner* owner)
    {
        assert(owner);
        _range = range;
        _owner = owner;
        owner._writeBorrowed = true;
    }

    @disable this(this);        // cannot be copied

    ~this()
    {
        _owner._writeBorrowed = false;
    }

private:
    Range _range;                   /// range
    Owner* _owner = null;           /// pointer to container owner
    alias _range this;              /// behave like range
}

/** Read-borrowed access to range `Range`. */
private static struct ReadBorrowedSlice(Range, Owner)
    // if (isInstanceOf!(Owned, Owner))
{
    this(const Range range, Owner* owner)
    {
        assert(owner);
        _range = range;
        _owner = owner;
        _owner._readerCount += 1;
    }

    this(this)
    {
        _owner._readerCount += 1;
    }

    ~this()
    {
        assert(_owner._readerCount != 0);
        _owner._readerCount -= 1;
    }

private:
    const Range _range;         /// constant range
    Owner* _owner = null;       /// pointer to container owner
    alias _range this;          /// behave like range
}

template needsOwnership(C)
{
    import std.range.primitives : hasSlicing;
    // TODO activate when array_ex : Array
    // enum needsOwnership = hasSlicing!C; // TODO extend to check if it's not @safe
    enum needsOwnership = is(C == struct);
}

pure unittest
{
    import std.exception: assertThrown;
    import core.exception : AssertError;

    import array_ex : Array;

    alias A = Array!int;

    Owned!A oa;

    oa ~= 1;
    oa ~= 2;
    assert(oa[] == [1, 2]);
    assert(!oa.writerBorrowed);
    assert(oa.readerCount == 0);

    {
        const wb = oa.writableSlice;
        assert(wb.length == 2);
        static assert(!__traits(compiles, { auto wc = wb; })); // write borrows cannot be copied
        assert(oa.writerBorrowed);
        assert(oa.readerCount == 0);
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }

    // ok to write borrow again in separate scope
    {
        const wb = oa.writableSlice;
        assert(wb.length == 2);
        assert(oa.writerBorrowed);
        assert(oa.readerCount == 0);
    }

    // multiple read-only borrows are allowed
    {
        const rb1 = oa.readOnlySlice;
        assert(rb1.length == oa.length);
        assert(oa.readerCount == 1);

        const rb2 = oa.readOnlySlice;
        assert(rb2.length == oa.length);
        assert(oa.readerCount == 2);

        const rb3 = oa.readOnlySlice;
        assert(rb3.length == oa.length);
        assert(oa.readerCount == 3);

        const rb_ = rb3;
        assert(rb_.length == oa.length);
        assert(oa.readerCount == 4);
        assertThrown!AssertError(oa.writableSlice); // single write borrow is not allowed
    }

    // test modification via write borrow
    {
        auto wb = oa.writableSlice;
        wb[0] = 11;
        wb[1] = 12;
        assert(wb.length == oa.length);
        assert(oa.writerBorrowed);
        assert(oa.readerCount == 0);
        assertThrown!AssertError(oa.readOnlySlice);
    }
    assert(oa[] == [11, 12]);

    // test writeable slice
    foreach (ref e; oa.writableSlice)
    {
        assertThrown!AssertError(oa.readOnlySlice); // one more write borrow is not allowed
        assertThrown!AssertError(oa.writableSlice); // one more write borrow is not allowed
        assertThrown!AssertError(oa[]); // one more write borrow is not allowed
    }

    // test readable slice
    foreach (const ref e; oa[])
    {
        assert(oa.readOnlySlice.length == oa.length);
        assertThrown!AssertError(oa.writableSlice); // write borrow during iteration is not allowed
    }

    // test moves
    auto oaMove1 = oa.move();
    auto oaMove2 = oaMove1.move();
    assert(oaMove2[] == [11, 12]);
}
