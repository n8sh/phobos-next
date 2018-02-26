module borrowed;

/** Write-borrowed access to range `Range`. */
struct WriteBorrowed(Range, Owner)
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
        debug assert(_owner._writeBorrowed, "Write borrow flag is already false, something is wrong with borrowing logic");
        _owner._writeBorrowed = false;
    }

    Range _range;                   /// range
    alias _range this;              /// behave like range

private:
    Owner* _owner = null;           /// pointer to container owner
}

/** Read-borrowed access to range `Range`. */
struct ReadBorrowed(Range, Owner)
    // if (isInstanceOf!(Owned, Owner))
{
    this(const Range range, Owner* owner)
    {
        import std.traits : Unqual;
        _range = *(cast(Unqual!Range*)&range);
        _owner = owner;
        if (_owner)
        {
            assert(_owner._readBorrowCount != _owner.readBorrowCountMax, "Cannot have more borrowers");
            _owner._readBorrowCount = _owner._readBorrowCount + 1;
        }
    }

    this(this)
    {
        if (_owner)
        {
            assert(_owner._readBorrowCount != _owner.readBorrowCountMax, "Cannot have more borrowers");
            _owner._readBorrowCount = _owner._readBorrowCount + 1;
        }
    }

    ~this()
    {
        if (_owner)
        {
            assert(_owner._readBorrowCount != 0, "Read borrow counter is already zero, something is wrong with borrowing logic");
            _owner._readBorrowCount = _owner._readBorrowCount - 1;
        }
    }

    /// Get read-only slice in range `i` .. `j`.
    auto opSlice(size_t i, size_t j)
    {
        return typeof(this)(_range[i .. j], _owner);
    }

    /// Get read-only slice.
    auto opSlice() inout
    {
        return this;            // same as copy
    }

    @property bool empty() const @safe pure nothrow @nogc
    {
        import std.range : empty; // pick this if `_range` doesn't have it
        return _range.empty;
    }

    @property auto ref front() inout @safe pure
    {
        assert(!empty);
        import std.range : front; // pick this if `_range` doesn't have it
        return _range.front;
    }

    typeof(this) save()         // forward range
    {
        return this;
    }

    void popFront() @safe
    {
        import std.range : popFront; // pick this if `_range` doesn't have it
        _range.popFront();
    }

    Range _range;               /// constant range
    alias _range this;          /// behave like range

private:
    Owner* _owner = null;       /// pointer to container owner
}
