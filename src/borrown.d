/** Ownership and borrwoing á lá Rust.
    TODO Move to typecons_ex.
 */
module borrown;

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

    ~this()
    {
        assert(!writeBorrowed, "Container is still write-borrowed, cannot release!");
    }

    WriteBorrowedRange!(Range, Owned) opSlice() @trusted
    {
        assert(!writeBorrowed, "Container is already write-borrowed!"); // exlusive writes
        writeBorrowed = true;
        return typeof(return)(_range.opSlice, &this);
    }

private:
    Container _range;               /// wrapped container
    bool writeBorrowed = 0;     /// `true' if _range is currently referred to
    uint readRangeCount = 0;    /// number of readable borrowers
    alias _range this;
}

import std.traits : isInstanceOf;

/** Write-borrowed access to range `Range`. */
struct WriteBorrowedRange(Range, Owner)
    if (isInstanceOf!(Owned, Owner))
{
    ~this()
    {
        _owner.writeBorrowed = false; // release borrow
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

    {
        auto wb1 = oa.opSlice;      // write borrow
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }
    {
        auto wb1 = oa.opSlice;      // write borrow
        assertThrown!AssertError(oa.opSlice); // one more write borrow is not allowed
    }
}
