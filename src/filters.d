module filters;

/// Growable flag.
enum Growable { no, yes }

/// Copyable flag.
enum Copyable { no, yes }

enum isDenseSetFilterable(E) = (is(typeof(cast(size_t)E.init)) && // is castable to size_t
                                cast(uint)E.max <= uint.max);      // and small enough

/** Store presence of elements of type `E` in a set in the range `0 .. length`.
    Can be seen as a generalization of `std.typecons.BitFlags` to integer types.

    Typically used to implement very fast membership checking. For instance in
    graph traversal algorithms, this filter is typically used as a temporary set
    node (integer) ids that checks if a node has been previously visisted or
    not.
 */
struct DenseSetFilter(E,
                      // TODO make these use the Flags template
                      Growable growable = Growable.yes,
                      Copyable copyable = Copyable.no)
    if (isDenseSetFilterable!E)
{
    import core.memory : malloc = pureMalloc, calloc = pureCalloc, realloc = pureRealloc;
    import core.bitop : bts, btr, btc, bt;

    @safe pure nothrow @nogc:

    /// Maximum number of elements in filter.
    enum elementMaxCount = cast(size_t)E.max + 1;

    /// Construct set to store at most `length` number of bits.
    this(size_t length) @trusted
    {
        _blocksPtr = null;
        static if (growable == Growable.yes)
        {
            _length = length;
            _capacity = 0;
            assureCapacity(length);
        }
        else
        {
            _capacity = length;
            _blocksPtr = cast(Block*)calloc(blockCount, Block.sizeof);
        }
    }

    static if (growable == Growable.no)
    {
        /// Construct from inferred capacity and length `elementMaxCount`.
        pragma(inline, true)
        static typeof(this) withInferredLength()
        {
            return typeof(return)(elementMaxCount);
        }
    }

    pragma(inline, true)
    ~this()
    {
        release();
    }

    /// Free storage.
    pragma(inline, true)
    private void release() @trusted
    {
        import qcmeman : free;
        free(_blocksPtr);
    }

    /// Clear contents.
    pragma(inline, true)
    void clear()
    {
        release();
        _blocksPtr = null;
        static if (growable == Growable.yes)
        {
            _length = 0;
        }
        _capacity = 0;
    }

    static if (copyable)
    {
        this(this) @trusted
        {
            Block* srcBlocksPtr = _blocksPtr;
            _blocksPtr = cast(Block*)malloc(blockCount * Block.sizeof);
            _blocksPtr[0 .. blockCount] = srcBlocksPtr[0 .. blockCount];
        }
    }
    else
    {
        @disable this(this);

        /// Returns: shallow (and deep) duplicate of `this`.
        typeof(this) dup() @trusted
        {
            typeof(return) copy;
            static if (growable == Growable.yes)
            {
                copy._length = this._length;
            }
            copy._capacity = this._capacity;
            copy._blocksPtr = cast(Block*)malloc(blockCount * Block.sizeof);
            copy._blocksPtr[0 .. blockCount] = this._blocksPtr[0 .. blockCount];
            return copy;
        }
    }

    @property:

    static if (growable == Growable.yes)
    {
        /// Expand to capacity to make room for at least `newLength`.
        private void assureCapacity(size_t newLength) @trusted
        {
            if (_capacity < newLength)
            {
                const oldBlockCount = blockCount;
                import std.math : nextPow2;
                this._capacity = newLength.nextPow2;
                _blocksPtr = cast(Block*)realloc(_blocksPtr,
                                                 blockCount * Block.sizeof);
                _blocksPtr[oldBlockCount .. blockCount] = 0;
            }
        }
    }

    /** Insert element `e`.
        Returns: precense status of element before insertion.
    */
    pragma(inline, true)
    bool insert(in E e) @trusted
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes)
        {
            assureCapacity(ix + 1);
            _length = ix + 1;
        }
        else
        {
            assert(ix < _capacity);
        }
        return bts(_blocksPtr, ix) != 0;
    }
    alias put = insert;         // OutputRange compatibility

    /** Remove element `e`.
        Returns: precense status of element before removal.
     */
    pragma(inline, true)
    bool remove(in E e) @trusted
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes)
        {
            assureCapacity(ix + 1);
            _length = ix + 1;
        }
        else
        {
            assert(ix < _capacity);
        }
        return btr(_blocksPtr, ix) != 0;
    }

    /** Insert element `e` if it's present otherwise remove it.
        Returns: `true` if elements was zeroed, `false` otherwise.
     */
    pragma(inline, true)
    bool complement(in E e) @trusted
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes)
        {
            assureCapacity(ix + 1);
            _length = ix + 1;
        }
        else
        {
            assert(ix < _capacity);
        }
        return btc(_blocksPtr, ix) != 0;
    }

    /// Check if element `e` is stored/contained.
    pragma(inline, true)
    bool contains(in E e) @trusted const
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes)
        {
            return ix < _length && bt(_blocksPtr, ix) != 0;
        }
        else
        {
            return ix < _capacity && bt(_blocksPtr, ix) != 0;
        }
    }
    /// ditto
    pragma(inline, true)
    auto opBinaryRight(string op)(in E e) const
        if (op == "in")
    {
        return contains(e);
    }

    /// ditto
    typeof(this) opBinary(string op)(auto ref in typeof(this) e) const
        if (op == "|" || op == "&" || op == "^")
    {
        typeof(return) result;
        mixin(`result._blocks[] = _blocks[] ` ~ op ~ ` e._blocks[];`);
        return result;
    }

    /** Get current capacity in number of elements (bits).
        If `growable` is `Growable.yes` then capacity is variable, otherwise it's constant.
    */
    pragma(inline, true)
    @property size_t capacity() const
    {
        return _capacity;
    }

private:
    pragma(inline, true)
    @property size_t blockCount() const
    {
        return _capacity / Block.sizeof + (_capacity % Block.sizeof ? 1 : 0);
    }

    alias Block = size_t;       /// Allocated block type.
    Block* _blocksPtr;          /// Pointer to blocks of bits.
    static if (growable == Growable.yes)
    {
        size_t _length;         /// Offset + 1 of highest set bit.
        size_t _capacity;       /// Number of bits allocated.
    }
    else
    {
        size_t _capacity;       /// Number of bits allocated.
    }
}

///
@safe pure nothrow @nogc unittest
{
    alias E = uint;

    import std.range : isOutputRange;
    alias Set = DenseSetFilter!(E, Growable.no);
    static assert(isOutputRange!(Set, E));

    const set0 = Set();
    assert(set0.capacity == 0);

    const length = 2^^6;
    auto set = DenseSetFilter!(E, Growable.no)(2*length);
    const y = set.dup;
    assert(y.capacity == 2*length);

    foreach (const ix; 0 .. length)
    {
        assert(!set.contains(ix));
        assert(ix !in set);

        assert(!set.insert(ix));
        assert(set.contains(ix));
        assert(ix in set);

        assert(set.complement(ix));
        assert(!set.contains(ix));
        assert(ix !in set);

        assert(!set.complement(ix));
        assert(set.contains(ix));
        assert(ix in set);

        assert(!set.contains(ix + 1));
    }

    auto z = set.dup;
    foreach (const ix; 0 .. length)
    {
        assert(z.contains(ix));
        assert(ix in z);
    }

    foreach (const ix; 0 .. length)
    {
        assert(set.contains(ix));
        assert(ix in set);
    }

    foreach (const ix; 0 .. length)
    {
        assert(set.contains(ix));
        set.remove(ix);
        assert(!set.contains(ix));
    }
}

///
@safe pure nothrow @nogc unittest
{
    alias E = uint;

    auto set = DenseSetFilter!(E, Growable.yes)();
    assert(set._length == 0);

    const length = 2^^16;
    foreach (const ix; 0 .. length)
    {
        assert(!set.contains(ix));
        assert(ix !in set);

        assert(!set.insert(ix));
        assert(set.contains(ix));
        assert(ix in set);

        assert(set.complement(ix));
        assert(!set.contains(ix));
        assert(ix !in set);

        assert(!set.complement(ix));
        assert(set.contains(ix));
        assert(ix in set);

        assert(!set.contains(ix + 1));
    }
}

/// test `RefCounted` storage
nothrow @nogc unittest          // TODO pure when https://github.com/dlang/phobos/pull/4692/files has been merged
{
    import std.typecons : RefCounted;
    alias E = int;

    RefCounted!(DenseSetFilter!(E, Growable.yes)) set;

    assert(set._length == 0);
    assert(set.capacity == 0);

    assert(!set.insert(0));
    assert(set._length == 1);
    assert(set.capacity == 2);

    const y = set;

    foreach (const e; 1 .. 1000)
    {
        assert(!set.insert(e));
        assert(set._length == e + 1);
        assert(y._length == e + 1);
    }

    const set1 = RefCounted!(DenseSetFilter!(E, Growable.yes))(42);
    assert(set1._length == 42);
    assert(set1.capacity == 64);
}

///
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d, dAlias = d }

    auto set = DenseSetFilter!(E, Growable.yes)();

    assert(set._length == 0);

    import std.traits : EnumMembers;
    foreach (const lang; [EnumMembers!E])
    {
        assert(!set.contains(lang));
    }
    foreach (const lang; [EnumMembers!E])
    {
        set.insert(lang);
        assert(set.contains(lang));
    }

}

///
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d, dAlias = d }

    auto set = DenseSetFilter!(E, Growable.no).withInferredLength(); // TODO use instantiator function here
    assert(set.capacity == typeof(set).elementMaxCount);

    static assert(!__traits(compiles, { assert(set.contains(0)); }));
    static assert(!__traits(compiles, { assert(set.insert(0)); }));
    static assert(!__traits(compiles, { assert(0 in set); }));

    import std.traits : EnumMembers;
    foreach (const lang; [EnumMembers!E])
    {
        assert(!set.contains(lang));
        assert(lang !in set);
    }
    foreach (const lang; [EnumMembers!E])
    {
        set.insert(lang);
        assert(set.contains(lang));
        assert(lang in set);
    }

}

/** Check if `E` is filterable in `StaticDenseSetFilter`, that is castable to
    `uint` and castable from unsigned int zero.
*/
template isStaticDenseFilterableType(E)
{
    import std.traits : hasIndirections, isUnsigned, isSomeChar;
    static if (is(E == enum) ||
               isUnsigned!E ||
               isSomeChar!E)
    {
        enum isStaticDenseFilterableType = true;
    }
    else static if (is(typeof(E.init.toUnsigned)))
    {
        alias UnsignedType = typeof(E.init.toUnsigned());
        enum isStaticDenseFilterableType = (isUnsigned!UnsignedType &&
                                            is(typeof(E.fromUnsigned(UnsignedType.init))) &&
                                            !hasIndirections!E);
    }
    else
    {
        enum isStaticDenseFilterableType = false;
    }
}

@safe pure nothrow @nogc unittest
{
    static assert(isStaticDenseFilterableType!uint);
    static assert(!isStaticDenseFilterableType!string);
    static assert(isStaticDenseFilterableType!char);
    static assert(!isStaticDenseFilterableType!(char*));

    enum E { a, b }
    static assert(isStaticDenseFilterableType!E);
}

/** Store presence of elements of type `E` in a set in the range `0 .. length`.
    Can be seen as a generalization of `std.typecons.BitFlags` to integer types.

    Typically used to implement very fast membership checking in sets of
    enumerators.

    TODO Add operators for bitwise `and` and `or` operations similar to
    https://dlang.org/library/std/typecons/bit_flags.html
 */
struct StaticDenseSetFilter(E,
                            bool requestPacked = true)
    if (isStaticDenseFilterableType!E)
{
    import std.range : isIterable, ElementType;
    import std.traits: isAssignable, isUnsigned;
    import core.bitop : bts, btr, btc, bt;

    alias This = typeof(this);

    @safe pure:

    string toString() const @property @trusted
    {
        import std.array : Appender;
        import std.conv : to;

        Appender!(typeof(return)) str = This.stringof ~ "([";
        bool other = false;

        pragma(inline, true)
        void putElement(in E e)
        {
            if (contains(e))
            {
                if (other)
                {
                    str.put(", ");
                }
                else
                {
                    other = true;
                }
                str.put(e.to!string);
            }
        }

        static if (is(E == enum))
        {
            import std.traits : EnumMembers;
            foreach (const e; [EnumMembers!E])
            {
                putElement(e);
            }
        }
        else
        {
            foreach (const i; E.unsignedMin .. E.unsignedMax + 1)
            {
                putElement(E.fromUnsigned(i));
            }
        }

        str.put("])");
        return str.data;
    }

    nothrow @nogc:

    /** Construct from elements `r`.
     */
    private this(R)(R r)
        if (isIterable!R &&
            isAssignable!(E, ElementType!R))
    {
        foreach (const ref e; r)
        {
            insert(e);
        }
    }

    /** Construct from `r` if `r` is non-empty, otherwise construct a full set.
     */
    static typeof(this) withValuesOrFull(R)(R r)
        if (isIterable!R &&
            isAssignable!(E, ElementType!R))
    {
        import std.range : empty;
        if (r.empty)
        {
            return asFull();
        }
        return typeof(return)(r);
    }

    pragma(inline, true):

    /// Construct a full set .
    static typeof(this) asFull()
    {
        typeof(return) that = void;
        that._blocks[] = Block.max;
        return that;
    }

    /** Insert element `e`.
     */
    void insert(in E e) @trusted
    {
        import bitop_ex : setBit;
        static if (isPackedInScalar)
        {
            _blocks[0].setBit(cast(size_t)e);
        }
        else
        {
            bts(_blocksPtr, cast(size_t)e);
        }
    }
    alias put = insert;         // OutputRange compatibility

    /** Remove element `e`.
     */
    void remove(in E e) @trusted
    {
        import bitop_ex : resetBit;
        static if (isPackedInScalar)
        {
            _blocks[0].resetBit(cast(size_t)e);
        }
        else
        {
            btr(_blocksPtr, cast(size_t)e);
        }
    }

    @property:

    /** Check if element `e` is present/stored/contained.
     */
    bool contains(in E e) @trusted const
    {
        import bitop_ex : testBit;
        static if (isPackedInScalar)
        {
            auto y = _blocks[0].testBit(cast(size_t)e);
            return y;
        }
        else
        {
            return bt(_blocksPtr, cast(size_t)e) != 0;
        }
    }

    /// ditto
    auto opBinaryRight(string op)(in E e) const
        if (op == "in")
    {
        return contains(e);
    }

    /// ditto
    typeof(this) opBinary(string op)(auto ref in typeof(this) e) const
        if (op == "|" || op == "&" || op == "^")
    {
        typeof(return) result;
        mixin(`result._blocks[] = _blocks[] ` ~ op ~ ` e._blocks[];`);
        return result;
    }

    /// ditto
    typeof(this) opOpAssign(string op)(auto ref in typeof(this) e)
        if (op == "|" || op == "&" || op == "^")
    {
        mixin(`_blocks[] ` ~ op ~ `= e._blocks[];`);
        return this;
    }

private:
    static if (is(E == enum) || isUnsigned!E) // has normal
    {
        /// Maximum number of elements in filter.
        enum elementMaxCount = E.max - E.min + 1;
    }
    else
    {
        /// Maximum number of elements in filter.
        enum elementMaxCount = E.unsignedMax - E.unsignedMin + 1;
    }

    static if (requestPacked)
    {
        static      if (elementMaxCount <= 8*ubyte.sizeof)
        {
            enum isPackedInScalar = true;
            alias Block = ubyte;
        }
        else static if (elementMaxCount <= 8*ushort.sizeof)
        {
            enum isPackedInScalar = true;
            alias Block = ushort;
        }
        else static if (elementMaxCount <= 8*uint.sizeof)
        {
            enum isPackedInScalar = true;
            alias Block = uint;
        }
        else
        {
            enum isPackedInScalar = false;
            alias Block = size_t;
        }
    }
    else
    {
        enum isPackedInScalar = false;
        alias Block = size_t;
    }

    /** Number of bits per `Block`. */
    enum bitsPerBlock = 8*Block.sizeof;

    /** Number of `Block`s. */
    enum blockCount = (elementMaxCount + (bitsPerBlock-1)) / bitsPerBlock;

    Block[blockCount] _blocks;          /// Pointer to blocks of bits.
    inout(Block)* _blocksPtr() @trusted inout { return _blocks.ptr; }
}

version(unittest)
{
    import std.traits : EnumMembers;
}

///
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d, dAlias = d }

    auto set = StaticDenseSetFilter!(E)();
    static assert(set.sizeof == 1);

    static assert(!__traits(compiles, { assert(set.contains(0)); }));
    static assert(!__traits(compiles, { assert(set.insert(0)); }));
    static assert(!__traits(compiles, { assert(0 in set); }));

    // initially empty
    foreach (const lang; [EnumMembers!E])
    {
        assert(!set.contains(lang));
        assert(lang !in set);
    }

    // insert
    foreach (const lang; [EnumMembers!E])
    {
        set.insert(lang);
        assert(set.contains(lang));
        assert(lang in set);
    }

    // remove
    foreach (const lang; [EnumMembers!E])
    {
        set.remove(lang);
        assert(!set.contains(lang));
        assert(lang !in set);
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d, dAlias = d }

    const E[2] es = [E.a, E.c];
    auto set = StaticDenseSetFilter!(E)(es[]);
    static assert(set.sizeof == 1);

    foreach (const ref e; es)
    {
        assert(set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d }

    const E[] es = [];
    auto set = StaticDenseSetFilter!(E).withValuesOrFull(es[]);
    static assert(set.sizeof == 1);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        assert(e in set);
        set.remove(e);
        assert(!set.contains(e));
        assert(e !in set);
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d }

    const E[2] es = [E.a, E.c];
    auto set = StaticDenseSetFilter!(E).withValuesOrFull(es[]);
    static assert(set.sizeof == 1);

    foreach (const ref e; es)
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte { a, b, c, d }

    auto set = StaticDenseSetFilter!(E).asFull;
    static assert(set.sizeof == 1);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte
    {
        a, b, c, d, e, f, g, h,
    }

    auto set = StaticDenseSetFilter!(E).asFull;
    static assert(set.sizeof == 1);
    static assert(set.isPackedInScalar);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte
    {
        a, b, c, d, e, f, g, h,
        i,
    }

    auto set = StaticDenseSetFilter!(E).asFull;
    static assert(set.sizeof == 2);
    static assert(set.isPackedInScalar);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte
    {
        a, b, c, d, e, f, g, h,
        i, j, k, l, m, n, o, p,
    }

    auto set = StaticDenseSetFilter!(E).asFull;
    static assert(set.sizeof == 2);
    static assert(set.isPackedInScalar);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte
    {
        a, b, c, d, e, f, g, h,
        i, j, k, l, m, n, o, p,
        r,
    }

    auto set = StaticDenseSetFilter!(E).asFull;
    static assert(set.sizeof == 4);
    static assert(set.isPackedInScalar);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

/// assignment from range
@safe pure nothrow @nogc unittest
{
    enum E:ubyte
    {
        a, b, c, d, e, f, g, h,
        i, j, k, l, m, n, o, p,
        r, s, t, u, v, w, x, y,
        z, a1, a2, a3, a4, a5, a6, a7,
        a8
    }

    auto set = StaticDenseSetFilter!(E).asFull;
    static assert(set.sizeof == 8);
    static assert(!set.isPackedInScalar);

    foreach (const e; [EnumMembers!E])
    {
        assert(set.contains(e));
        set.remove(e);
        assert(!set.contains(e));
    }
}

version(unittest)
{
    enum Rel : ubyte { unknown, subClassOf, instanceOf, memberOf }
    import bit_traits : packedBitSizeOf;
    static assert(packedBitSizeOf!Rel == 2);

    struct Role
    {
        Rel rel;
        bool reversion;

        enum unsignedMin = 0;
        enum unsignedMax = 2^^(1 +  // reversion
                               packedBitSizeOf!Rel // relation
            ) - 1;

        alias UnsignedType = ushort;
        static assert(typeof(this).sizeof == UnsignedType.sizeof);
        static assert(Role.sizeof == 2);

        pragma(inline, true)
        @safe pure nothrow @nogc:

        /// Create from `UnsignedType` `u`.
        static typeof(this) fromUnsigned(in UnsignedType u) @trusted
        {
            typeof(return) that;
            that.reversion = (u >> 0) & 1;
            that.rel = cast(Rel)(u >> 1);
            return that;
        }

        /// Convert to `UnsignedType` `u`.
        UnsignedType toUnsigned() const @trusted
        {
            return cast(UnsignedType)((cast(UnsignedType)reversion << 0) |
                                      (cast(UnsignedType)rel << 1));
        }

        UnsignedType opCast(UnsignedType)() const @nogc
        {
            return toUnsigned;
        }
    }

    static assert(is(typeof(Role.init.toUnsigned)));
    static assert(is(typeof(Role.init.toUnsigned()) == Role.UnsignedType));
    static assert(isStaticDenseFilterableType!Role);
}

/// assignment from range
@safe pure unittest
{
    auto set = StaticDenseSetFilter!(Role)();
    static assert(set.sizeof == 1);

    assert(set.toString == "StaticDenseSetFilter!(Role, true)([])");

    // inserts
    foreach (const rel; [EnumMembers!Rel])
    {
        assert(!set.contains(Role(rel)));
        assert(!set.contains(Role(rel, true)));

        set.insert(Role(rel));
        assert(set.contains(Role(rel)));

        assert(!set.contains(Role(rel, true)));
        set.insert(Role(rel, true));

        assert(set.contains(Role(rel)));
        assert(set.contains(Role(rel, true)));
    }

    assert(set.toString == "StaticDenseSetFilter!(Role, true)([const(Role)(unknown, false), const(Role)(unknown, true), const(Role)(subClassOf, false), const(Role)(subClassOf, true), const(Role)(instanceOf, false), const(Role)(instanceOf, true), const(Role)(memberOf, false), const(Role)(memberOf, true)])");

    // removes
    foreach (const rel; [EnumMembers!Rel])
    {
        assert(set.contains(Role(rel)));
        assert(set.contains(Role(rel, true)));

        set.remove(Role(rel));
        assert(!set.contains(Role(rel)));

        assert(set.contains(Role(rel, true)));
        set.remove(Role(rel, true));

        assert(!set.contains(Role(rel)));
        assert(!set.contains(Role(rel, true)));
    }

    assert(set.toString == "StaticDenseSetFilter!(Role, true)([])");

    auto fullSet = StaticDenseSetFilter!(Role).asFull;

    foreach (const rel; [EnumMembers!Rel])
    {
        assert(fullSet.contains(Role(rel)));
        assert(fullSet.contains(Role(rel, true)));
    }

    auto emptySet = StaticDenseSetFilter!(Role)();

    foreach (const rel; [EnumMembers!Rel])
    {
        assert(!emptySet.contains(Role(rel)));
        assert(!emptySet.contains(Role(rel, true)));
    }
}

/// set operations
@safe pure nothrow @nogc unittest
{
    import array_help : s;

    enum E:ubyte { a, b, c, d, dAlias = d }

    auto a = StaticDenseSetFilter!(E)([E.a].s[]);
    auto b = StaticDenseSetFilter!(E)([E.b].s[]);
    auto c = StaticDenseSetFilter!(E)([E.b].s[]);

    auto a_or_b = StaticDenseSetFilter!(E)([E.a, E.b].s[]);
    auto a_and_b = StaticDenseSetFilter!(E)();

    assert((a | b | c) == a_or_b);
    assert((a & b) == a_and_b);
    assert((a & a & b) == a_and_b);

    a |= b;
    assert(a == a_or_b);
}
