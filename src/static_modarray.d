module static_modarray;

version = useModulo;

/** Statically allocated `Mod`-array of fixed pre-allocated length `capacity` of
    `Mod`-elements in chunks of `elementLength`. `ElementType` is
    `Mod[elementLength]`.
*/
struct StaticModArray(uint capacity,
                      uint elementLength = 1,
                      uint span = 8)
    if (capacity*elementLength >= 2) // no use storing less than 2 bytes
{
    private enum radix = 2^^span;

    /// Index modulo `radix` type.
    version(useModulo)
    {
        import modulo : Mod;
        alias Ix = Mod!(radix, ubyte);
    }
    else
    {
        alias Ix = ubyte;
    }

    enum L = elementLength;

    /// ElementType type `T`.
    static if (L == 1)
    {
        alias T = Ix;
    }
    else
    {
        alias T = Ix[L];
    }

    /** Construct with `rhsCapacity`. */
    this(uint rhsCapacity)(in StaticModArray!(rhsCapacity,
                                              elementLength,
                                              span) rhs)
    {
        static if (capacity < rhsCapacity)
        {
            assert(rhs.length <= capacity);
        }
        foreach (immutable i, const ix; rhs)
        {
            _store[i] = ix;
        }
        _length = rhs.length;
    }

    /** Construct with elements `es`. */
    this(Es...)(Es es)
        if (Es.length >= 1 &&
            Es.length <= capacity)
    {
        foreach (immutable i, const ix; es)
        {
            _store[i] = ix;
        }
        _length = es.length;
    }

    static if (L == 1)
    {
        /** Construct with elements in `es`. */
        this(const Ix[] es)
        {
            assert(es.length <= capacity);
            _store[0 .. es.length] = es;
            _length = es.length;
        }
    }

    /** Default key separator in printing. */
    enum keySeparator = ',';

    @property auto toString(char separator = keySeparator) const
    {
        string s;
        foreach (immutable i, const ix; chunks)
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
    auto front() inout
    {
        pragma(inline, true);
        assert(!empty);
        return _store[0];
    }

    /** Get last element. */
    auto back() inout
    {
        pragma(inline, true);
        assert(!empty);
        return _store[_length - 1];
    }

    /** Returns: `true` if `this` is empty, `false` otherwise. */
    bool empty() const
    {
        pragma(inline, true);
        return _length == 0;
    }

    /** Returns: `true` if `this` is full, `false` otherwise. */
    bool full() const
    {
        pragma(inline, true);
        return _length == capacity;
    }

    /** Pop first (front) element. */
    auto ref popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (immutable i; 0 .. _length - 1)
        {
            _store[i] = _store[i + 1]; // like `_store[i] = _store[i + 1];` but more generic
        }
        _length = _length - 1;
        return this;
    }

    /** Pop `n` front elements. */
    auto ref popFrontN(size_t n)
    {
        assert(length >= n);
        // TODO is there a reusable Phobos function for this?
        foreach (immutable i; 0 .. _length - n)
        {
            _store[i] = _store[i + n];
        }
        _length = _length - n;
        return this;
    }

    /** Pop last (back) element. */
    auto ref popBack()
    {
        version(LDC) pragma(inline, true);
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
        foreach (immutable i, const e; es)
        {
            _store[_length + i] = e;
        }
        _length = _length + Es.length;
        return this;
    }

    /** Returns: `true` if `key` is contained in `this`. */
    bool contains(in Ix[] key) const @nogc
    {
        pragma(inline, true);
        // TODO use binarySearch instead of canFind
        import std.algorithm.searching : canFind;
        if (key.length != L) { return false; }
        return (chunks.canFind(key));
    }

    static if (L == 1)
    {
        import std.traits : isUnsigned;

        /** Returns: `true` if `ix` is contained in `this`. */
        version(useModulo)
        {
            bool contains(ModUInt)(in Mod!(radix, ModUInt) ix) const @nogc
                if (isUnsigned!ModUInt)
            {
                pragma(inline, true);
                // TODO use binarySearch instead of canFind
                import std.algorithm.searching : canFind;
                return (chunks.canFind(ix));
            }
        }
        else
        {
            bool contains(UInt)(in UInt ix) const @nogc
                if (isUnsigned!UInt)
            {
                pragma(inline, true);
                // TODO use binarySearch instead of canFind
                import std.algorithm.searching : canFind;
                return (chunks.canFind(cast(T)ix));
            }
        }
    }

    /** Returns: elements as a slice. */
    auto chunks() inout
    {
        pragma(inline, true);
        return _store[0 .. _length];
    }
    alias chunks this;

    /** Variant of `opIndex` with compile-time range checking. */
    auto ref at(uint ix)() inout @trusted
        if (ix < capacity)      // assert below memory allocation bound
    {
        pragma(inline, true);
        assert(ix < _length);   // assert accessing initialized elements
        return _store.ptr[ix];    // uses `.ptr` because `ix` known at compile-time to be within bounds; `ix < capacity`
    }

    /** Get length. */
    auto length() const
    {
        pragma(inline, true);
        return _length;
    }

    /** Get remaining space available.
        Name taken from the same member of https://docs.rs/fixedvec/0.2.3/fixedvec/
     */
    auto available() const
    {
        pragma(inline, true);
        return capacity - _length;
    }

    enum typeBits = 4; // number of bits in enclosing type used for representing type

private:
    static if (L == 1)
    {
        T[capacity] _store = void; // byte indexes
    }
    else
    {
        T[capacity] _store = void; // byte indexes
    }

    static if (_store.sizeof == 6)
    {
        ubyte _padding;
    }

    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_length", 4, // maximum length of 15
                     ubyte, "_mustBeIgnored", typeBits)); // must be here and ignored because it contains `WordVariant` type of `Node`
}

static assert(StaticModArray!(3, 1, 8).sizeof == 4);
static assert(StaticModArray!(7, 1, 8).sizeof == 8);
static assert(StaticModArray!(3, 2, 8).sizeof == 8);
static assert(StaticModArray!(2, 3, 8).sizeof == 8);

///
@safe pure nothrow @nogc unittest
{
    import std.algorithm : equal;

    version(useModulo)
    {
        enum span = 8;
        enum radix = 2^^span;
        import modulo : Mod, mod;
        alias Ix = Mod!(radix, ubyte);
        static Mod!radix mk(ubyte value)
        {
            return mod!radix(value);
        }
    }
    else
    {
        alias Ix = ubyte;
        static ubyte mk(ubyte value)
        {
            return value;
        }
    }

    const ixs = [mk(11), mk(22), mk(33), mk(44)].s;
    enum capacity = 7;

    auto x = StaticModArray!(capacity, 1)(ixs);
    auto y = StaticModArray!(capacity, 1)(mk(11), mk(22), mk(33), mk(44));

    assert(x == y);

    assert(x.length == 4);
    assert(x.available == 3);
    assert(!x.empty);

    assert(!x.contains([mk(10)].s));
    assert(x.contains([mk(11)].s));
    assert(x.contains([mk(22)].s));
    assert(x.contains([mk(33)].s));
    assert(x.contains([mk(44)].s));
    assert(!x.contains([mk(45)].s));

    assert(!x.contains(mk(10)));
    assert(x.contains(mk(11)));
    assert(x.contains(mk(22)));
    assert(x.contains(mk(33)));
    assert(x.contains(mk(44)));
    assert(!x.contains(mk(45)));

    assert(x.equal([11, 22, 33, 44].s[]));
    assert(x.front == 11);
    assert(x.back == 44);
    assert(!x.full);
    x.popFront();
    assert(x.equal([22, 33, 44].s[]));
    assert(x.front == 22);
    assert(x.back == 44);
    assert(!x.full);
    x.popBack();
    assert(x.equal([22, 33].s[]));
    assert(x.front == 22);
    assert(x.back == 33);
    assert(!x.full);
    x.popFront();
    assert(x.equal([33].s[]));
    assert(x.front == 33);
    assert(x.back == 33);
    assert(!x.full);
    x.popFront();
    assert(x.empty);
    assert(!x.full);
    assert(x.length == 0);

    x.pushBack(mk(11), mk(22), mk(33), mk(44), mk(55), mk(66), mk(77));
    assert(x.equal([11, 22, 33, 44, 55, 66, 77].s[]));
    assert(!x.empty);
    assert(x.full);

    x.popFrontN(3);
    assert(x.equal([44, 55, 66, 77].s[]));

    x.popFrontN(2);
    assert(x.equal([66, 77].s[]));

    x.popFrontN(1);
    assert(x.equal([77].s[]));

    x.popFrontN(1);
    assert(x.empty);

    x.pushBack(mk(1)).pushBack(mk(2)).equal([1, 2].s[]);
    assert(x.equal([1, 2].s[]));
    assert(x.length == 2);
}

@safe pure nothrow unittest
{
    import std.algorithm : equal;

    version(useModulo)
    {
        enum span = 8;
        enum radix = 2^^span;
        import modulo : Mod, mod;
        alias Ix = Mod!(radix, ubyte);
        static Mod!radix mk(ubyte value)
        {
            return mod!radix(value);
        }
    }
    else
    {
        alias Ix = ubyte;
        static ubyte mk(ubyte value)
        {
            return value;
        }
    }

    const ixs = [mk(11), mk(22), mk(33), mk(44)].s;
    enum capacity = 7;
    auto z = StaticModArray!(capacity, 1)(ixs);
    assert(z.sizeof == 8);
    try
    {
        assert(z.toString == `0B,16,21,2C`);
    }
    catch (Exception e) {}
}

version(unittest)
{
    import array_help : s;
}
