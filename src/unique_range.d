module unique_range;

version(unittest)
{
    import dbgio : dln;
    import std.algorithm.comparison : equal;
}

import std.range.primitives : hasLength;

/** Unique range (slice) owning its source of `Source`.

    Copy construction is disabled, explicit copying is instead done through
    member `.dup`.
 */
struct UniqueRange(Source)
    if (hasLength!Source)       // TODO use traits `isArrayContainer` checking fo
{
    import std.range : ElementType, isBidirectionalRange;
    import std.traits : isArray;
    alias SourceRange = typeof(Source.init[]);
    alias E = ElementType!SourceRange;

    @disable this(this);        // not intended to be copied

    pragma(inline, true) @safe pure nothrow @nogc:

    /// Construct from `source`.
    this(Source source)
    {
        import std.algorithm.mutation : move;
        move(source, _source); // TODO remove `move` when compiler does it for us
        _sourceRange = _source[];
    }

    /// Construct from reference to `source`, used by `intoUniqueRange`.
    private this(ref Source source)
    {
        import std.algorithm.mutation : move;
        move(source, _source); // TODO remove `move` when compiler does it for us
        _sourceRange = _source[];
    }

    /// Is `true` if range is empty.
    @property bool empty() const
    {
        static if (!__traits(hasMember, SourceRange, "empty"))
        {
            import std.range : empty;
        }
        return _sourceRange.empty;
    }

    /// Front element.
    @property scope auto ref inout(E) front() inout return @trusted
    {
        assert(!empty);
        static if (!__traits(hasMember, SourceRange, "front"))
        {
            import std.range : front;
        }
        return cast(inout(E))(cast(SourceRange)_sourceRange).front;
    }

    /// Pop front element.
    void popFront()
    {
        static if (!__traits(hasMember, SourceRange, "popFront"))
        {
            import std.range : popFront;
        }
        _sourceRange.popFront(); // should include check for emptyness
    }

    /// Pop front element and return it.
    E frontPop()()
    {
        assert(!empty);

        import std.algorithm.mutation : move;
        import std.traits : isCopyable, hasElaborateDestructor;
        static if (isCopyable!E &&
                   !hasElaborateDestructor!E)
        {
            typeof(return) value = front;
            popFront();
            return value;
        }
        else
        {
            static assert(false, "TODO if front is an l-value move it out and return it");
            // import std.traits : Unqual;
            // TODO reinterpret as typeof(*(cast(Unqual!E*)(&_source[_frontIx]))) iff `E` doesn't contain any immutable indirections
            // typeof(return) value = move(_sourceRange.front);
            // popFront();
            // return value;
        }
    }
    alias stealFront = frontPop;

    static if (isBidirectionalRange!(typeof(Source.init[])))
    {
        /// Back element.
        @property scope auto ref inout(E) back() inout return @trusted
        {
            assert(!empty);
            static if (!__traits(hasMember, SourceRange, "back"))
            {
                import std.range : back;
            }
            return cast(inout(E))(cast(SourceRange)_sourceRange).back;
        }

        /// Pop back element.
        void popBack()
        {
            static if (!__traits(hasMember, SourceRange, "popBack"))
            {
                import std.range : popBack;
            }
            _sourceRange.popBack(); // should include check for emptyness
        }

        /// Pop back element and return it.
        E backPop()()
        {
            assert(!empty);

            import std.algorithm.mutation : move;
            import std.traits : isCopyable, hasElaborateDestructor;
            static if (isCopyable!E &&
                       !hasElaborateDestructor!E)
            {
                typeof(return) value = back;
                popBack();
                return value;
            }
            else
            {
                static assert(false, "TODO if back is an l-value move it out and return it");
                // import std.traits : Unqual;
                // TODO reinterpret as typeof(*(cast(Unqual!E*)(&_source[_backIx]))) iff `E` doesn't contain any immutable indirections
                // typeof(return) value = move(_sourceRange.back);
                // popBack();
                // return value;
            }
        }
        alias stealBack = backPop;
    }

    /// Returns: shallow duplicate of `this`.
    version(none)               // TODO make compile
    {
        @property UniqueRange dup() const
        {
            return typeof(this)(_source.dup);
        }
    }

    /// Returns: length of `this`.
    static if (hasLength!(typeof(Source.init[])))
    {
        @property size_t length() const { return _sourceRange.length; }
    }

private:
    Source _source; // typically a non-reference count container type with disable copy construction
    SourceRange _sourceRange;
}

/** Returns: A range of `Source` that owns its `source` (data container).
    Similar to Rust's `into_iter`.
 */
UniqueRange!Source intoUniqueRange(Source)(Source source)
    if (hasLength!Source)
{
    return typeof(return)(source); // construct from reference
}

/// A generator is a range which owns its state (typically a non-reference counted container).
alias intoGenerator = intoUniqueRange;

/// basics
@safe pure nothrow @nogc unittest
{
    import basic_array : SA = BasicArray;
    import std.range.primitives : isInputRange, isIterable;
    alias C = SA!int;

    auto ba = C.withLength(4);
    ba[0 .. 4] = [11, 13, 15, 17].s[];
    import std.algorithm.mutation : move;
    auto cs = move(ba).intoUniqueRange; // TODO withElements()

    static assert(isInputRange!(typeof(cs)));
    static assert(isIterable!(typeof(cs)));

    assert(!cs.empty);
    assert(cs.length == 4);
    assert(cs.front == 11);
    assert(cs.back == 17);

    cs.popFront();
    assert(cs.length == 3);
    assert(cs.front == 13);
    assert(cs.back == 17);

    cs.popBack();
    assert(cs.length == 2);
    assert(cs.front == 13);
    assert(cs.back == 15);

    assert(cs.frontPop() == 13);
    assert(cs.length == 1);
    assert(cs.front == 15);
    assert(cs.back == 15);

    assert(cs.backPop() == 15);
    assert(cs.length == 0);
    assert(cs.empty);
}

/// combined with Phobos ranges
@safe pure nothrow unittest
{
    import basic_array : SA = BasicArray;
    assert(SA!int([11, 13, 15, 17].s[])
           .intoUniqueRange()
           .filterUnique!(_ => _ != 11)
           .mapUnique!(_ => 2*_)
           .equal([2*13, 2*15, 2*17]));
}

import std.functional : unaryFun;

template mapUnique(fun...) if (fun.length >= 1)
{
    import std.algorithm.mutation : move;
    import std.range.primitives : isInputRange, ElementType;
    import std.traits : Unqual;

    auto mapUnique(Range)(Range r) if (isInputRange!(Unqual!Range))
    {
        import std.meta : AliasSeq, staticMap;

        alias RE = ElementType!(Range);
        static if (fun.length > 1)
        {
            import std.functional : adjoin;
            import std.meta : staticIndexOf;

            alias _funs = staticMap!(unaryFun, fun);
            alias _fun = adjoin!_funs;

            // Once DMD issue #5710 is fixed, this validation loop can be moved into a template.
            foreach (f; _funs)
            {
                static assert(!is(typeof(f(RE.init)) == void),
                    "Mapping function(s) must not return void: " ~ _funs.stringof);
            }
        }
        else
        {
            alias _fun = unaryFun!fun;
            alias _funs = AliasSeq!(_fun);

            // Do the validation separately for single parameters due to DMD issue #15777.
            static assert(!is(typeof(_fun(RE.init)) == void),
                "Mapping function(s) must not return void: " ~ _funs.stringof);
        }

        return MapUniqueResult!(_fun, Range)(move(r));
    }
}

private struct MapUniqueResult(alias fun, Range)
{
    import std.traits : Unqual, isCopyable;
    import std.range.primitives : isInputRange, isForwardRange, isBidirectionalRange, isRandomAccessRange, isInfinite, hasSlicing;
    import std.algorithm.mutation : move;

    alias R = Unqual!Range;
    R _input;

    static if (isBidirectionalRange!R)
    {
        @property auto ref back()()
        {
            assert(!empty, "Attempting to fetch the back of an empty mapUnique.");
            return fun(_input.back);
        }

        void popBack()()
        {
            assert(!empty, "Attempting to popBack an empty mapUnique.");
            _input.popBack();
        }
    }

    this(R input)
    {
        _input = move(input); // TODO remove `move` when compiler does it for us
    }

    static if (isInfinite!R)
    {
        // Propagate infinite-ness.
        enum bool empty = false;
    }
    else
    {
        @property bool empty()
        {
            return _input.empty;
        }
    }

    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty mapUnique.");
        _input.popFront();
    }

    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty mapUnique.");
        return fun(_input.front);
    }

    static if (isRandomAccessRange!R)
    {
        static if (is(typeof(_input[ulong.max])))
            private alias opIndex_t = ulong;
        else
            private alias opIndex_t = uint;

        auto ref opIndex(opIndex_t index)
        {
            return fun(_input[index]);
        }
    }

    static if (hasLength!R)
    {
        @property auto length()
        {
            return _input.length;
        }

        alias opDollar = length;
    }

    static if (hasSlicing!R &&
               isCopyable!R)
    {
        static if (is(typeof(_input[ulong.max .. ulong.max])))
            private alias opSlice_t = ulong;
        else
            private alias opSlice_t = uint;

        static if (hasLength!R)
        {
            auto opSlice(opSlice_t low, opSlice_t high)
            {
                return typeof(this)(_input[low .. high]);
            }
        }
        else static if (is(typeof(_input[opSlice_t.max .. $])))
        {
            struct DollarToken{}
            enum opDollar = DollarToken.init;
            auto opSlice(opSlice_t low, DollarToken)
            {
                return typeof(this)(_input[low .. $]);
            }

            auto opSlice(opSlice_t low, opSlice_t high)
            {
                import std.range : takeExactly;
                return this[low .. $].takeExactly(high - low);
            }
        }
    }

    static if (isForwardRange!R &&
               isCopyable!R)    // TODO should save be allowed for non-copyable?
    {
        @property auto save()
        {
            return typeof(this)(_input.save);
        }
    }
}

// TODO Add duck-typed interface that shows that result is still sorted according to `predicate`
template filterUnique(alias predicate) if (is(typeof(unaryFun!predicate)))
{
    import std.algorithm.mutation : move;
    import std.range.primitives : isInputRange;
    import std.traits : Unqual;

    auto filterUnique(Range)(Range range)
        if (isInputRange!(Unqual!Range))
    {
        return FilterUniqueResult!(unaryFun!predicate, Range)(move(range));
    }
}

// TODO Add duck-typed interface that shows that result is still sorted according to `predicate`
private struct FilterUniqueResult(alias pred, Range)
{
    import std.algorithm.mutation : move;
    import std.range.primitives : isForwardRange, isInfinite;
    import std.traits : Unqual, isCopyable;
    alias R = Unqual!Range;
    R _input;

    this(R r)
    {
        _input = move(r);       // TODO remove `move` when compiler does it for us
        while (!_input.empty && !pred(_input.front))
        {
            _input.popFront();
        }
    }

    static if (isCopyable!Range)
    {
        auto opSlice() { return this; }
    }

    static if (isInfinite!Range)
    {
        enum bool empty = false;
    }
    else
    {
        @property bool empty() { return _input.empty; }
    }

    void popFront()
    {
        do
        {
            _input.popFront();
        } while (!_input.empty && !pred(_input.front));
    }

    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty filterUnique.");
        return _input.front;
    }

    static if (isForwardRange!R &&
               isCopyable!R) // TODO should save be allowed for non-copyable?
    {
        @property auto save()
        {
            return typeof(this)(_input.save);
        }
    }
}

// TODO move these hidden behind template defs of takeUnique
import std.typecons : Unqual;
import std.range.primitives : isInputRange, isInfinite, hasSlicing;

/// Unique take.
UniqueTake!R takeUnique(R)(R input, size_t n)
    if (is(R T == UniqueTake!T))
{
    import std.algorithm.mutation : move;
    import std.algorithm.comparison : min;
    return R(move(input.source), // TODO remove `move` when compiler does it for us
             min(n, input._maxAvailable));
}

/// ditto
UniqueTake!(R) takeUnique(R)(R input, size_t n)
    if (isInputRange!(Unqual!R) &&
        (isInfinite!(Unqual!R) ||
         !hasSlicing!(Unqual!R) &&
         !is(R T == UniqueTake!T)))
{
    import std.algorithm.mutation : move;
    return UniqueTake!R(move(input), n); // TODO remove `move` when compiler does it for us
}

struct UniqueTake(Range)
    if (isInputRange!(Unqual!Range) &&
        //take _cannot_ test hasSlicing on infinite ranges, because hasSlicing uses
        //take for slicing infinite ranges.
        !((!isInfinite!(Unqual!Range) && hasSlicing!(Unqual!Range)) || is(Range T == UniqueTake!T)))
{
    import std.range.primitives : isForwardRange, hasAssignableElements, ElementType, hasMobileElements, isRandomAccessRange, moveFront;

    private alias R = Unqual!Range;

    /// User accessible in read and write
    public R source;

    private size_t _maxAvailable;

    alias Source = R;

    this(R source, size_t _maxAvailable)
    {
        import std.algorithm.mutation : move;
        this.source = move(source);
        this._maxAvailable = _maxAvailable;
    }

    /// Range primitives
    @property bool empty()
    {
        return _maxAvailable == 0 || source.empty;
    }

    /// ditto
    @property auto ref front()
    {
        assert(!empty,
            "Attempting to fetch the front of an empty "
            ~ UniqueTake.stringof);
        return source.front;
    }

    /// ditto
    void popFront()
    {
        assert(!empty,
            "Attempting to popFront() past the end of a "
            ~ UniqueTake.stringof);
        source.popFront();
        --_maxAvailable;
    }

    static if (isForwardRange!R)
        /// ditto
        @property UniqueTake save()
        {
            return UniqueTake(source.save, _maxAvailable);
        }

    static if (hasAssignableElements!R)
        /// ditto
        @property auto front(ElementType!R v)
        {
            assert(!empty,
                "Attempting to assign to the front of an empty "
                ~ UniqueTake.stringof);
            // This has to return auto instead of void because of Bug 4706.
            source.front = v;
        }

    // static if (hasMobileElements!R)
    // {
    //     /// ditto
    //     auto moveFront()
    //     {
    //         assert(!empty,
    //             "Attempting to move the front of an empty "
    //             ~ UniqueTake.stringof);
    //         return source.moveFront();
    //     }
    // }

    static if (isInfinite!R)
    {
        /// ditto
        @property size_t length() const
        {
            return _maxAvailable;
        }

        /// ditto
        alias opDollar = length;

        //Note: Due to UniqueTake/hasSlicing circular dependency,
        //This needs to be a restrained template.
        /// ditto
        auto opSlice()(size_t i, size_t j)
        if (hasSlicing!R)
        {
            assert(i <= j, "Invalid slice bounds");
            assert(j <= length, "Attempting to slice past the end of a "
                ~ UniqueTake.stringof);
            return source[i .. j];
        }
    }
    else static if (hasLength!R)
    {
        /// ditto
        @property size_t length()
        {
            import std.algorithm.comparison : min;
            return min(_maxAvailable, source.length);
        }

        alias opDollar = length;
    }

    static if (isRandomAccessRange!R)
    {
        /// ditto
        void popBack()
        {
            assert(!empty,
                "Attempting to popBack() past the beginning of a "
                ~ UniqueTake.stringof);
            --_maxAvailable;
        }

        /// ditto
        @property auto ref back()
        {
            assert(!empty,
                "Attempting to fetch the back of an empty "
                ~ UniqueTake.stringof);
            return source[this.length - 1];
        }

        /// ditto
        auto ref opIndex(size_t index)
        {
            assert(index < length,
                "Attempting to index out of the bounds of a "
                ~ UniqueTake.stringof);
            return source[index];
        }

        static if (hasAssignableElements!R)
        {
            /// ditto
            @property auto back(ElementType!R v)
            {
                // This has to return auto instead of void because of Bug 4706.
                assert(!empty,
                    "Attempting to assign to the back of an empty "
                    ~ UniqueTake.stringof);
                source[this.length - 1] = v;
            }

            /// ditto
            void opIndexAssign(ElementType!R v, size_t index)
            {
                assert(index < length,
                    "Attempting to index out of the bounds of a "
                    ~ UniqueTake.stringof);
                source[index] = v;
            }
        }

        static if (hasMobileElements!R)
        {
            /// ditto
            auto moveBack()
            {
                assert(!empty,
                    "Attempting to move the back of an empty "
                    ~ UniqueTake.stringof);
                return source.moveAt(this.length - 1);
            }

            /// ditto
            auto moveAt(size_t index)
            {
                assert(index < length,
                    "Attempting to index out of the bounds of a "
                    ~ UniqueTake.stringof);
                return source.moveAt(index);
            }
        }
    }

    /**
    Access to maximal length of the range.
    Note: the actual length of the range depends on the underlying range.
    If it has fewer elements, it will stop before maxLength is reached.
    */
    @property size_t maxLength() const
    {
        return _maxAvailable;
    }
}

/// array range
@safe pure nothrow @nogc unittest
{
    import basic_array : SA = BasicArray;
    import std.range.primitives : isInputRange, isIterable;
    alias C = SA!int;

    auto ba = C.withLength(2);
    ba[0 .. 2] = [11, 13].s[];
    import std.algorithm.mutation : move;
    auto cs = move(ba).intoUniqueRange; // TODO withElements()

    assert(cs.front == 11);
    cs.popFront();

    assert(cs.front == 13);
    cs.popFront();

    assert(cs.empty);

    static assert(isInputRange!(typeof(cs)));
    static assert(isIterable!(typeof(cs)));
}

/// hashset range
@safe pure nothrow @nogc unittest
{
    import std.algorithm.mutation : move;
    import hashset : HashSet;

    class C {}

    alias S = HashSet!C;
    S s;

    auto cs = move(s).intoUniqueRange;
}

import std.functional : binaryFun;

InputRange findUnique(alias pred = "a == b", InputRange, Element)(InputRange haystack, scope Element needle)
    if (isInputRange!InputRange &&
        is (typeof(binaryFun!pred(haystack.front, needle)) : bool))
{
    for (; !haystack.empty; haystack.popFront())
    {
        if (binaryFun!pred(haystack.front, needle))
            break;
    }
    import std.algorithm.mutation : move;
    return move(haystack);
}

version(unittest)
{
    import array_help : s;
}
