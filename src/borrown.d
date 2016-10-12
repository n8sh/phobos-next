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

/** Write-borrowed access to range `R`. */
struct WriteBorrowed(R, C)
{
    R _r;
    C* _c = null;                      // container
}

/** Return wrapper around container `C` that can be safely sliced, by tracking if
    write and read borrowed ranges.

    Only relevant when container `C` implements reference access over
    - `opSlice` and
    - `opIndex`
 */
struct Owned(C)
    if (needsOwnership!C)
{
    alias SliceType = typeof(C.init[]);
    WriteBorrowed!(SliceType, C) opSlice() @trusted
    {
        assert(!writeBorrowed, "Container already sliced!"); // exlusive writes
        writeBorrowed = true;
        return typeof(return)(_r.opSlice, &_r);
    }
private:
    C _r;                       /// wrapped container
    bool writeBorrowed = 0;     /// `true' if _r is currently referred to
    uint readRangeCount = 0;    /// number of readable borrowers
    alias _r this;
}

pure unittest
{
    import std.exception: assertThrown;
    import core.exception;

    import array_ex : Array;

    alias A = Array!int;

    Owned!A oa;

    auto wb1 = oa.opSlice;
    assertThrown!AssertError(oa.opSlice);
}
