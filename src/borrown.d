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

    // TODO can we somehow disallow move construction for Owned by checking if a  is run?

    ~this()
    {
        assert(!_writeBorrowed, "Container is still write-borrowed, cannot release!");
        assert(_readerCount == 0, "Container is still read-borrowed, cannot release!");
    }

    typeof(this) move()
    {
        assert(!_writeBorrowed, "Container is still write-borrowed, cannot move!");
        assert(_readerCount == 0, "Container is still read-borrowed, cannot move!");
        import std.algorithm.mutation : move;
        return move(this);
    }

    ReadBorrowedRange!(Range, Owned) readOnlySlice() @trusted
    {
        assert(!_writeBorrowed, "Container is write-borrowed!");
        _readerCount += 1;  // TODO move to ctor
        return typeof(return)(_range.opSlice, &this);
    }

    WriteBorrowedRange!(Range, Owned) writableSlice() @trusted
    {
        assert(!_writeBorrowed, "Container is already write-borrowed!");
        assert(_readerCount == 0, "Container is already read-borrowed!");
        _writeBorrowed = true;  // TODO move to ctor
        return typeof(return)(_range.opSlice, &this);
    }

    alias opSlice = writableSlice; // TODO default to read or write?

    @safe pure nothrow @nogc pragma(inline):

    @property:

    bool writerBorrowed() const { return _writeBorrowed; }
    uint readerCount() const { return _readerCount; }

private:
    Container _range;           /// wrapped container
    bool _writeBorrowed = false;     /// `true' if _range is currently referred to
    uint _readerCount = 0;   /// number of readable borrowers
    alias _range this;
}

import std.traits : isInstanceOf;

/** Write-borrowed access to range `Range`. */
private static struct WriteBorrowedRange(Range, Owner)
    if (isInstanceOf!(Owned, Owner))
{
    this(Range range, Owner* owner)
    {
        assert(owner);          // always non-null
        _range = range;
        _owner = owner;
    }

    ~this()
    {
        _owner._writeBorrowed = false; // release borrow
    }

    pragma(msg, "Active copy ctor!");
    // this(this)
    // {
    //     static assert(false, "Cannot have more than one writable borrow!");
    // }

private:
    Range _range;                   /// range
    Owner* _owner = null;           /// pointer to container owner
    alias _range this;              /// behave like range
}

/** Read-borrowed access to range `Range`. */
private static struct ReadBorrowedRange(Range, Owner)
    if (isInstanceOf!(Owned, Owner))
{
    this(Range range, Owner* owner)
    {
        assert(owner);          // always non-null
        _range = range;
        _owner = owner;
    }

    ~this()
    {
        assert(_owner._readerCount != 0);
        _owner._readerCount -= 1;
    }

    this(this)
    {
        _owner._readerCount += 1;
    }

private:
    Range _range;                   /// range
    Owner* _owner = null;           /// pointer to container owner
    alias _range this;              /// behave like range
}

pure unittest
{
    import std.exception: assertThrown;
    import core.exception;

    import array_ex : Array;

    alias A = Array!int;

    Owned!A oa;
    oa ~= 1;
    oa ~= 2;
    assert(oa[] == [1, 2]);
    assert(!oa.writerBorrowed);
    assert(oa.readerCount == 0);

    {
        auto wb = oa.opSlice;      // write borrow
        assert(oa.writerBorrowed);
        assert(oa.readerCount == 0);
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }

    // ok to write borrow again in separate scope
    {
        auto wb = oa.opSlice;      // write borrow
        assert(oa.writerBorrowed);
        assert(oa.readerCount == 0);
    }

    {
        auto rb1 = oa.readOnlySlice;
        assert(oa.readerCount == 1);
        auto rb2 = oa.readOnlySlice;
        assert(oa.readerCount == 2);
        auto rb3 = oa.readOnlySlice;
        assert(oa.readerCount == 3);
        auto rb_ = rb3;
        assert(oa.readerCount == 4);
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }

    // ok to write borrow again in separate scope
    {
        auto wb = oa.opSlice;      // write borrow
        assert(oa.writerBorrowed);
        assert(oa.readerCount == 0);
        assertThrown!AssertError(oa.readOnlySlice);
    }

    auto oaMove1 = oa.move();
    auto oaMove2 = oaMove1.move();
}
