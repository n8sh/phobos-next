/** Ownership and borrwoing á lá Rust.
    TODO Move to typecons_ex.
 */
module borrown;

version(unittest)
{
    import dbgio;
}

template needsOwnership(C)
{
    import std.range.primitives : hasSlicing;
    // enum needsOwnership = hasSlicing!C; // TODO extend to check if it's not @safe
    enum needsOwnership = true;
}

/** Return wrapper around container `Container` that can be safely sliced, by
    tracking number of read borrowed ranges and whether it's currently write
    borrowed.

    Only relevant when `Container` implements referenced access over
    - `opSlice` and
    - `opIndex`
*/
struct Owned(Container)
    if (needsOwnership!Container)
{
    /// Type of range of `Container`.
    alias Range = typeof(Container.init[]);

    // TODO can we somehow disallow move construction for Owned?

    ~this()
    {
        dln(writeBorrowed);
        dln(readBorrowCount);
        assert(!_writeBorrowed, "Container is still write-borrowed, cannot release!");
        assert(_readBorrowCount == 0, "Container is still read-borrowed, cannot release!");
    }

    WriteBorrowedRange!(Range, Owned) opSlice() @trusted
    {
        assert(!_writeBorrowed, "Container is already write-borrowed!");
        assert(_readBorrowCount == 0, "Container is already read-borrowed!");
        _writeBorrowed = true;  // TODO move to ctor
        return typeof(return)(_range.opSlice, &this);
    }

    ReadBorrowedRange!(Range, Owned) readOnlySlice() @trusted
    {
        assert(!_writeBorrowed, "Container is write-borrowed!");
        _readBorrowCount += 1;  // TODO move to ctor
        return typeof(return)(_range.opSlice, &this);
    }

    @safe pure nothrow @nogc pragma(inline):

    @property:

    bool writeBorrowed() const { return _writeBorrowed; }
    uint readBorrowCount() const { return _readBorrowCount; }

private:
    Container _range;           /// wrapped container
    bool _writeBorrowed = false;     /// `true' if _range is currently referred to
    uint _readBorrowCount = 0;   /// number of readable borrowers
    alias _range this;
}

import std.traits : isInstanceOf;

/** Write-borrowed access to range `Range`. */
struct WriteBorrowedRange(Range, Owner)
    if (isInstanceOf!(Owned, Owner))
{
    ~this()
    {
        _owner._writeBorrowed = false; // release borrow
    }

private:
    Range _range;                   /// range
    Owner* _owner = null;           /// pointer to container owner
    alias _range this;              /// behave like range
}

/** Read-borrowed access to range `Range`. */
struct ReadBorrowedRange(Range, Owner)
    if (isInstanceOf!(Owned, Owner))
{
    ~this()
    {
        assert(_owner._readBorrowCount != 0);
        _owner._readBorrowCount -= 1;
    }

private:
    Range _range;                   /// range
    Owner* _owner = null;           /// pointer to container owner
    alias _range this;              /// behave like range
}

pure unittest
{
    import std.algorithm : move;
    import std.exception: assertThrown;
    import core.exception;

    import array_ex : Array;

    alias A = Array!int;

    Owned!A oa;
    oa ~= 1;
    oa ~= 2;
    assert(oa[] == [1, 2]);

    {
        auto wb = oa.opSlice;      // write borrow
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }

    // ok to write borrow again in separate scope
    {
        auto wb = oa.opSlice;      // write borrow
    }

    {
        auto rb1 = oa.readOnlySlice;
        auto rb2 = oa.readOnlySlice;
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }

    // ok to write borrow again in separate scope
    {
        auto wb = oa.opSlice;      // write borrow
        assertThrown!AssertError(oa.readOnlySlice);
    }

    // TODO this will fail
    // auto oaMove1 = move(oa);
}
