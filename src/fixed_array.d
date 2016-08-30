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
    import modulo : Mod;
    import std.algorithm : move;

    alias Ix = Mod!(radix, ubyte);
    alias UIx = Mod!(radix, uint);

    enum L = elementLength;

    /// ElementType type `Element`.
    static if (L == 1)
        alias Element = Ix;
    else
        alias Element = Ix[L];

    this(Es...)(Es ixs)
        if (Es.length >= 1 &&
            Es.length <= capacity)
    {
        foreach (const i, const ix; ixs)
        {
            static assert(!is(typeof(ix) == int));
            _ixs[i] = ix;
        }
        _length = ixs.length;
    }

    static if (L == 1)
    {
        this(const Ix[] ixs)
        {
            assert(ixs.length <= capacity);
            _ixs[0 .. ixs.length] = ixs;
            _length = ixs.length;
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

    @safe pure nothrow @nogc:

    /** Get first element. */
    auto front() inout          // TODO should throw?
    {
        assert(!empty);
        return _ixs[0];
    }

    /** Get last element. */
    auto back() inout           // TODO should throw?
    {
        assert(!empty);
        return _ixs[_length - 1];
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
            move(_ixs[i + 1], _ixs[i]); // like `_ixs[i] = _ixs[i + 1];` but more generic
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
            move(_ixs[i + n], _ixs[i]); // like `_ixs[i] = _ixs[i + n];` but more generic
        }
        _length = _length - n;
        return this;
    }

    /** Pop last (back) element. */
    auto ref popBack()
    {
        assert(!empty);
        // TODO destruct last element?
        _length = _length - 1;
        return this;
    }

    /** Push/Add elements `moreEs` at back. */
    auto ref pushBack(Es...)(Es moreEs)
        if (Es.length <= capacity)
        {
            assert(length + Es.length <= capacity);
            foreach (const i, const ix; moreEs)
            {
                _ixs[_length + i] = ix;
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
        /** Returns: `true` if `ix` is contained in `this`. */
        bool contains(const UIx ix) const @nogc
        {
            // TODO use binarySearch instead of canFind
            import std.algorithm.searching : canFind;
            return (chunks.canFind(ix));
        }
    }

    auto chunks() inout { return _ixs[0 .. _length]; }
    alias chunks this;

    /** Variant of `opIndex` with compile-time range checking. */
    auto ref at(uint ix)() inout @trusted
        if (ix < capacity)      // assert below memory allocation bound
    {
        assert(ix < _length);   // assert accessing initialized elements
        return _ixs.ptr[ix];    // uses `.ptr` because `ix` known at compile to be within bounds; `ix < capacity`
    }

    /** Get length. */
    auto length() const { return _length; }

    enum typeBits = 4; // number of bits in enclosing type used for representing type

private:
    static if (L == 1)
    {
        Ix[capacity] _ixs;     // byte indexes
    }
    else
    {
        Ix[L][capacity] _ixs;  // byte indexes
    }

    static if (_ixs.sizeof == 6)
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
    assert(!x.empty);

    assert(!x.contains([10.mod!M]));
    assert(x.contains([11.mod!M]));
    assert(x.contains([22.mod!M]));
    assert(x.contains([33.mod!M]));
    assert(x.contains([44.mod!M]));
    assert(!x.contains([45.mod!M]));

    assert(x.equal([11, 22, 33, 44]));
    assert(x.front == 11);
    assert(x.back == 44);
    assert(!x.full);
    x.popFront();
    assert(x.equal([22, 33, 44]));
    assert(x.front == 22);
    assert(x.back == 44);
    assert(!x.full);
    x.popBack;
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
}
