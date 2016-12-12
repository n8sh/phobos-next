module fixed_array;

/** Statically allocated `Mod`-array of fixed pre-allocated length `capacity` of
    `Mod`-elements in chunks of `elementLength`. `ElementType` is
    `Mod[elementLength]`.
*/
struct ModArrayN(uint capacity,
                 uint elementLength = 1,
                 uint span = 8)
    if (capacity*elementLength >= 2) // no use storing less than 2 bytes
{
    private enum radix = 2^^span;
    import std.algorithm : move;

    static if (true) // TODO debug
    {
        import modulo : Mod;
        alias Ix = Mod!(radix, ubyte);
        alias UIx = Mod!(radix, uint);
    }
    else
    {
        alias Ix = ubyte;
        alias UIx = uint;
    }

    enum L = elementLength;

    /// ElementType type `Element`.
    static if (L == 1)
        alias Element = Ix;
    else
        alias Element = Ix[L];

    this(uint rhsCapacity)(in ModArrayN!(rhsCapacity, elementLength, span) rhs)
    {
        static if (capacity < rhsCapacity)
        {
            assert(rhs.length <= capacity);
        }
        foreach (const i, const ix; rhs)
        {
            _store[i] = ix;
        }
        _length = rhs.length;
    }

    pragma(inline) this(Es...)(Es es)
        if (Es.length >= 1 &&
            Es.length <= capacity)
    {
        foreach (const i, const ix; es)
        {
            _store[i] = ix;
        }
        _length = es.length;
    }

    static if (L == 1)
    {
        pragma(inline) this(const Ix[] es)
        {
            assert(es.length <= capacity);
            _store[0 .. es.length] = es;
            _length = es.length;
        }
    }

    enum keySeparator = ',';

    @property auto toString(char separator = keySeparator) const
    {
        string s;
        foreach (const i, const ix; chunks)
        {
            if (i != 0) { s ~= separator; }
            import std.string : format;
            static if (elementLength == 1)
            {
                s ~= format("%.2X", ix); // in hexadecimal
            }
            else
            {
                foreach (const j, const subIx; ix[])
                {
                    if (j != 0) { s ~= '_'; } // separator
                    s ~= format("%.2X", subIx); // in hexadecimal
                }
            }
        }
        return s;
    }

    pragma(inline) @safe pure nothrow @nogc:

    /** Get first element. */
    auto front() inout
    {
        assert(!empty);
        return _store[0];
    }

    /** Get last element. */
    auto back() inout
    {
        assert(!empty);
        return _store[_length - 1];
    }

    /** Returns: `true` if `this` is empty, `false` otherwise. */
    bool empty() const { return _length == 0; }

    /** Returns: `true` if `this` is full, `false` otherwise. */
    bool full() const { return _length == capacity; }

    /** Pop first (front) element. */
    auto ref popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - 1)
        {
            move(_store[i + 1], _store[i]); // like `_store[i] = _store[i + 1];` but more generic
        }
        _length = _length - 1;
        return this;
    }

    /** Pop `n` front elements. */
    auto ref popFrontN(size_t n)
    {
        assert(length >= n);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - n)
        {
            move(_store[i + n], _store[i]); // like `_store[i] = _store[i + n];` but more generic
        }
        _length = _length - n;
        return this;
    }

    /** Pop last (back) element. */
    auto ref popBack()
    {
        assert(!empty);
        _length = cast(typeof(_length))(_length - 1); // TODO better?
        return this;
    }

    /** Push/Add elements `es` at back.
        NOTE Doesn't invalidate any borrow.
    */
    auto ref pushBack(Es...)(Es es)
        if (Es.length <= capacity)
    {
        assert(length + Es.length <= capacity);
        foreach (const i, ref e; es)
        {
            import std.algorithm.mutation : move;
            _store[_length + i] = e.move();
        }
        _length = _length + Es.length;
        return this;
    }

    /** Returns: `true` if `key` is contained in `this`. */
    bool contains(const Ix[] key) const @nogc
    {
        // TODO use binarySearch instead of canFind
        import std.algorithm.searching : canFind;
        if (key.length != L) { return false; }
        return (chunks.canFind(key));
    }

    static if (L == 1)
    {
        import std.traits : isUnsigned;

        /** Returns: `true` if `ix` is contained in `this`. */
        static if (true) // TODO debug
        {
            bool contains(ModUInt)(const Mod!(radix, ModUInt) ix) const @nogc
                if (isUnsigned!ModUInt)
            {
                // TODO use binarySearch instead of canFind
                import std.algorithm.searching : canFind;
                return (chunks.canFind(ix));
            }
        }
        else
        {
            bool contains(UInt)(const UInt ix) const @nogc
                if (isUnsigned!UInt)
            {
                // TODO use binarySearch instead of canFind
                import std.algorithm.searching : canFind;
                return (chunks.canFind(ix));
            }
        }
    }

    /** Returns: elements as a slice. */
    auto chunks() inout { return _store[0 .. _length]; }
    alias chunks this;

    /** Variant of `opIndex` with compile-time range checking. */
    auto ref at(uint ix)() inout @trusted
        if (ix < capacity)      // assert below memory allocation bound
    {
        assert(ix < _length);   // assert accessing initialized elements
        return _store.ptr[ix];    // uses `.ptr` because `ix` known at compile-time to be within bounds; `ix < capacity`
    }

    /** Get length. */
    auto length() const { return _length; }

    /** Get remaining space available.
        Name taken from the same member of https://docs.rs/fixedvec/0.2.3/fixedvec/
     */
    auto available() const { return capacity - _length; }

    enum typeBits = 4; // number of bits in enclosing type used for representing type

private:
    static if (L == 1)
    {
        Ix[capacity] _store = void; // byte indexes
    }
    else
    {
        Ix[L][capacity] _store = void; // byte indexes
    }

    static if (_store.sizeof == 6)
    {
        ubyte _padding;
    }

    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_length", 4, // maximum length of 15
                     ubyte, "_mustBeIgnored", typeBits)); // must be here and ignored because it contains `WordVariant` type of `Node`
}

static assert(ModArrayN!(3, 1, 8).sizeof == 4);
static assert(ModArrayN!(7, 1, 8).sizeof == 8);
static assert(ModArrayN!(3, 2, 8).sizeof == 8);
static assert(ModArrayN!(2, 3, 8).sizeof == 8);

///
@safe pure nothrow unittest
{
    import std.algorithm : equal;
    import modulo : Mod, mod;

    enum span = 8;
    enum M = 2^^span;

    alias Ix = Mod!(M, ubyte);
    Ix[] ixs = [11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M];
    enum capacity = 7;

    auto x = ModArrayN!(capacity, 1)(ixs);
    auto y = ModArrayN!(capacity, 1)(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M);

    assert(x == y);

    assert(x.length == 4);
    assert(x.available == 3);
    assert(!x.empty);

    assert(!x.contains([10.mod!M]));
    assert(x.contains([11.mod!M]));
    assert(x.contains([22.mod!M]));
    assert(x.contains([33.mod!M]));
    assert(x.contains([44.mod!M]));
    assert(!x.contains([45.mod!M]));

    assert(!x.contains(10.mod!M));
    assert(x.contains(11.mod!M));
    assert(x.contains(22.mod!M));
    assert(x.contains(33.mod!M));
    assert(x.contains(44.mod!M));
    assert(!x.contains(45.mod!M));

    assert(x.equal([11, 22, 33, 44]));
    assert(x.front == 11);
    assert(x.back == 44);
    assert(!x.full);
    x.popFront();
    assert(x.equal([22, 33, 44]));
    assert(x.front == 22);
    assert(x.back == 44);
    assert(!x.full);
    x.popBack();
    assert(x.equal([22, 33]));
    assert(x.front == 22);
    assert(x.back == 33);
    assert(!x.full);
    x.popFront();
    assert(x.equal([33]));
    assert(x.front == 33);
    assert(x.back == 33);
    assert(!x.full);
    x.popFront();
    assert(x.empty);
    assert(!x.full);
    assert(x.length == 0);

    x.pushBack(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M, 55.mod!M, 66.mod!M, 77.mod!M);
    assert(x.equal([11, 22, 33, 44, 55, 66, 77]));
    assert(!x.empty);
    assert(x.full);

    x.popFrontN(3);
    assert(x.equal([44, 55, 66, 77]));

    x.popFrontN(2);
    assert(x.equal([66, 77]));

    x.popFrontN(1);
    assert(x.equal([77]));

    x.popFrontN(1);
    assert(x.empty);

    x.pushBack(1).pushBack(2).equal([1, 2]);
    assert(x.equal([1, 2]));
    assert(x.length == 2);
}

///
@safe pure unittest
{
    import modulo : Mod, mod;

    enum span = 8;
    enum M = 2^^span;

    alias Ix = Mod!(M, ubyte);
    Ix[] ixs = [11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M];
    enum capacity = 7;
    auto x = ModArrayN!(capacity, 1)(ixs);
    assert(x.sizeof == 8);

    assert(x.toString == `0B,16,21,2C`);
}
