#!/usr/bin/env rdmd-dev-module

/** Extensions to std.range.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
*/

module range_ex;

import std.range: hasSlicing, hasLength, isSomeString, isNarrowString, isInfinite, isInputRange, isBidirectionalRange, ElementType;
import std.traits: hasUnsharedAliasing, hasElaborateDestructor, isScalarType;

public import slicing;

enum hasPureCopy(T) = (isScalarType!T || // TODO remove?
                       (!hasUnsharedAliasing!T &&
                        !hasElaborateDestructor!T));

enum hasStealableElements(R) = (hasPureCopy!(ElementType!R)); // TODO recurse

/* template hasStealableElements(T...) */
/* { */
/*     import std.range: ElementType; */
/*     import std.typecons : Rebindable; */

/*     static if (is(ElementType!T)) */
/*     { */
/*         enum hasStealableElements = true; */
/*     } */
/*     else static if (is(T[0] R: Rebindable!R)) */
/*     { */
/*         enum hasStealableElements = hasStealableElements!R; */
/*     } */
/*     else */
/*     { */
/*         template unsharedDelegate(T) */
/*         { */
/*             enum bool unsharedDelegate = isDelegate!T */
/*             && !is(T == shared) */
/*             && !is(T == shared) */
/*             && !is(T == immutable) */
/*             && !is(FunctionTypeOf!T == shared) */
/*             && !is(FunctionTypeOf!T == immutable); */
/*         } */

/*         enum hasStealableElements = */
/*         hasRawUnsharedAliasing!(T[0]) || */
/*         anySatisfy!(unsharedDelegate, RepresentationTypeTuple!(T[0])) || */
/*         hasUnsharedObjects!(T[0]) || */
/*         hasStealableElements!(T[1..$]); */
/*     } */
/* } */

/** Steal front from $(D r) destructively and return it.
   See also: http://forum.dlang.org/thread/jkbhlezbcrufowxtthmy@forum.dlang.org#post-konhvblwbmpdrbeqhyuv:40forum.dlang.org
   See also: http://forum.dlang.org/thread/onibkzepudfisxtrigsi@forum.dlang.org#post-dafmzroxvaeejyxrkbon:40forum.dlang.org
*/
auto stealFront(R)(ref R r)
    if (isInputRange!R &&
        hasStealableElements!R)
{
    import std.range: moveFront, popFront;
    /* scope(success) r.popFront(); */
    /* return r.moveFront(); */
    auto e = r.moveFront();
    r.popFront();
    return e;
}
alias pullFront = stealFront;
alias takeFront = stealFront;

@safe pure nothrow unittest
{
    auto x = [11, 22];
    assert(x.stealFront == 11); assert(x == [22]);
    assert(x.stealFront == 22); assert(x == []);
}

@safe pure nothrow unittest
{
    auto x = ["a", "b"];
    assert(x.stealFront == "a"); assert(x == ["b"]);
}

@safe pure nothrow unittest
{
    struct V { int x, y; }
    auto x = [V(11, 12),
              V(21, 22)];
    assert(x.stealFront == V(11, 12)); assert(x == [V(21, 22)]);
    assert(x.stealFront == V(21, 22)); assert(x == []);
}

/** Steal back from $(D r) destructively and return it.
    See also: http://forum.dlang.org/thread/jkbhlezbcrufowxtthmy@forum.dlang.org#post-konhvblwbmpdrbeqhyuv:40forum.dlang.org
    See also: http://forum.dlang.org/thread/onibkzepudfisxtrigsi@forum.dlang.org#post-dafmzroxvaeejyxrkbon:40forum.dlang.org
*/
auto stealBack(R)(ref R r)
    if (isBidirectionalRange!R &&
        hasStealableElements!R)
{
    import std.range: moveBack, popBack;
    /* scope(success) r.popBack(); */
    /* return r.moveBack; */
    auto e = r.moveBack;
    r.popBack();
    return e;
}
alias pullBack = stealBack;
alias takeBack = stealBack;

@safe pure nothrow unittest
{
    auto x = [11, 22];
    assert(x.stealBack == 22); assert(x == [11]);
    assert(x.stealBack == 11); assert(x == []);
}

@safe pure nothrow unittest
{
    auto x = ["a", "b"];
    assert(x.stealBack == "b"); assert(x == ["a"]);
}

@safe pure nothrow unittest
{
    struct V { int x, y; }
    auto x = [V(11, 12),
              V(21, 22)];
    assert(x.stealBack == V(21, 22)); assert(x == [V(11, 12)]);
    assert(x.stealBack == V(11, 12)); assert(x == []);
}

/** Sliding Splitter.
    See also: http://forum.dlang.org/thread/dndicafxfubzmndehzux@forum.dlang.org
    See also: http://forum.dlang.org/thread/uzrbmjonrkixojzflbig@forum.dlang.org#epost-viwkavbmwouiquoqwntm:40forum.dlang.org

    TODO Use size_t for _lower and _upper instead and reserve _upper =
    size_t.max for emptyness?

    TODO Should lower and upper operate on code units instead of code
    point if isNarrowString!Range. ?

    TODO generalize with stride
*/
struct SlidingSplitter(Range)
    if (isSomeString!Range ||
        (hasSlicing!Range &&
         !isInfinite!Range))
{
    import std.range: isForwardRange;
    import std.typecons: Unqual, Tuple, tuple;
    alias R = Unqual!Range;

    this(R)(R data, size_t lower = 0)
    in { assert(lower <= data.length); }
    body
    {
        _data = data;
        static if (hasSlicing!Range) // TODO should we use isSomeString here instead?
        {
            _lower = lower;
            _upper = data.length;
        }
        else
        {
            while (lower)
            {
                popFront;
                --lower;
            }
        }
        _upper = data.length;
    }

    this(R)(R data, size_t lower, size_t upper)
    in { assert(lower <= upper + 1 || // the extra + 1 makes empty initialization (lower + 1 == upper) possible in for example opSlice below
                ((lower <= data.length) &&
                 (upper <= data.length))); }
    body
    {
        _data = data;
        _lower = lower;
        _upper = upper;
    }

    @property Tuple!(R, R) front()
    {
        return typeof(return)(_data[0 .. _lower],
                              _data[_lower .. $]);
    }

    void popFront()
    {
        static if (isNarrowString!R)
        {
            if (_lower < _upper)
            {
                import std.utf: stride;
                _lower += stride(_data, _lower);
            }
            else                // when we can't decode beyond
            {
                ++_lower; // so just indicate we're beyond back
            }
        }
        else
        {
            ++_lower;
        }
    }

    static if (!isInfinite!R)
    {
        @property Tuple!(R, R) back()
        {
            return typeof(return)(_data[0 .. _upper],
                                  _data[_upper .. $]);
        }
        void popBack()
        {
            static if (isNarrowString!R)
            {
                if (_lower < _upper)
                {
                    import std.utf: strideBack;
                    _upper -= strideBack(_data, _upper);
                }
                else                // when we can't decode beyond
                {
                    --_upper; // so just indicate we're beyond front
                }
            }
            else
            {
                --_upper;
            }
        }
    }

    static if (isForwardRange!R)
    {
        @property auto save()
        {
            import std.range: save;
            return typeof(this)(_data.save, _lower, _upper);
        }
    }

    static if (isInfinite!R)
    {
        enum bool empty = false;  // propagate infiniteness
    }
    else
    {
        @property bool empty() const
        {
            return _upper < _lower;
        }
    }

    static if (hasSlicing!R)
    {
        Tuple!(R, R) opIndex(size_t i)
        in { assert(i < length); }
        body
        {
            return typeof(return)(_data[0 .. _lower + i],
                                  _data[_lower + i .. _upper]);
        }

        typeof(this) opSlice(size_t lower, size_t upper)
        {
            if (lower == upper)
            {
                return slidingSplitter(_data,
                                       _upper + 1, // defines empty intialization
                                       _upper);
            }
            else
            {
                return slidingSplitter(_data,
                                       _lower + lower,
                                       _lower + (upper - 1));
            }
        }

        // TODO Should length be provided if isNarrowString!Range?
        @property size_t length() const
        {
            return _upper - _lower + 1;
        }
    }

    private R _data;
    private ptrdiff_t _lower;
    private ptrdiff_t _upper;
}

auto slidingSplitter(R)(R data, size_t lower = 0)
{
    return SlidingSplitter!R(data, lower, data.length);
}

auto slidingSplitter(R)(R data, size_t lower, size_t upper)
{
    return SlidingSplitter!R(data, lower, upper);
}

@safe pure nothrow unittest
{
    import std.typecons : tuple;
    import std.conv: to;

    auto x = [1, 2, 3];

    import std.range: isInputRange, isForwardRange, isBidirectionalRange, isRandomAccessRange;

    static assert(isInputRange!(SlidingSplitter!(typeof(x))));
    static assert(isForwardRange!(SlidingSplitter!(typeof(x))));
    // static assert(isBidirectionalRange!(SlidingSplitter!(typeof(x))));
    static assert(isRandomAccessRange!(SlidingSplitter!(typeof(x))));
    static assert(!isRandomAccessRange!(SlidingSplitter!string));
    static assert(!isRandomAccessRange!(SlidingSplitter!wstring));
    static assert(isRandomAccessRange!(SlidingSplitter!dstring));

    auto y = SlidingSplitter!(typeof(x))(x);

    for (size_t i; i < y.length; ++i)
    {
        assert(y[i] == tuple(x[0..i], x[i..3]));
    }

    assert(y.front == tuple([], x));
    assert(!y.empty);
    assert(x.length + 1 == y.length);

    assert(!y.empty); assert(y.front == tuple(x[0 .. 0], x[0 .. 3])); y.popFront();
    assert(!y.empty); assert(y.front == tuple(x[0 .. 1], x[1 .. 3])); y.popFront();
    assert(!y.empty); assert(y.front == tuple(x[0 .. 2], x[2 .. 3])); y.popFront();
    assert(!y.empty); assert(y.front == tuple(x[0 .. 3], x[3 .. 3])); y.popFront();
    y.popFront(); assert(y.empty);
}

@safe pure unittest                        // forwards
{
    import std.conv: to;

    size_t lower = 2;

    auto name = "Nordlöw";
    auto name8  = name.to! string.slidingSplitter(lower);
    auto name16 = name.to!wstring.slidingSplitter(lower);
    auto name32 = name.to!dstring.slidingSplitter(lower);

    static assert(!__traits(compiles, { name8.length >= 0; } ));
    static assert(!__traits(compiles, { name16.length >= 0; } ));
    assert(name32.length);

    foreach (ch; name8)
    {
        foreach (ix; iota!(0, ch.length)) // for each part in split
        {
            import std.algorithm: equal;
            assert(ch[ix].equal(name16.front[ix]));
            assert(ch[ix].equal(name32.front[ix]));

        }
        name16.popFront();
        name32.popFront();
    }
}

@safe pure unittest                        // backwards
{
    import std.conv: to;
    import std.range: retro;

    size_t lower = 2;

    auto name = "Nordlöw";
    auto name8  = name.to! string.slidingSplitter(lower).retro;
    auto name16 = name.to!wstring.slidingSplitter(lower).retro;
    auto name32 = name.to!dstring.slidingSplitter(lower).retro;

    foreach (ch; name8)
    {
        foreach (ix; iota!(0, ch.length)) // for each part in split
        {
            import std.algorithm: equal;
            assert(ch[ix].equal(name16.front[ix]));
            assert(ch[ix].equal(name32.front[ix]));
        }
        name16.popFront();
        name32.popFront();
    }
}

@safe pure nothrow unittest                        // radial
{
    auto x = [1, 2, 3];
    import std.range: radial;
    import std.typecons : tuple;
    auto s = x.slidingSplitter;
    auto r = s.radial;
    assert(!r.empty); assert(r.front == tuple(x[0 .. 1], x[1 .. 3])); r.popFront();
    assert(!r.empty); assert(r.front == tuple(x[0 .. 2], x[2 .. 3])); r.popFront();
    assert(!r.empty); assert(r.front == tuple(x[0 .. 0], x[0 .. 3])); r.popFront();
    assert(!r.empty); assert(r.front == tuple(x[0 .. 3], x[3 .. 3])); r.popFront();
    assert(r.empty);
}

/** Ring Buffer.
    See also: http://forum.dlang.org/thread/ltpaqk$2dav$1@digitalmars.com
    TODO inout
 */
struct RingBuffer(T)
{
    this(T[] data, size_t length = 0)
    {
        enforce(data.length, "empty ring buffer is prohibited");
        enforce(length <= data.length, "buffer length shall not be more
than buffer capacity");
        _data = data;
        _beginIndex = 0;
        _length = length;
    }

    auto opSlice() const
    {
	return cycle(_data[0 .. _length]).take(_length);
    }

    @property auto length() { return _length; }

private:
    T[] _data;
    size_t _beginIndex;
    size_t _length;
}

/** Static Iota.
    TODO Add to Phobos.
*/
template iota(size_t from, size_t to)
    if (from <= to)
{
    alias iota = siotaImpl!(to-1, from);
}
private template siotaImpl(size_t to, size_t now)
{
    import std.meta: AliasSeq;
    static if (now >= to) { alias siotaImpl = AliasSeq!(now); }
    else                  { alias siotaImpl = AliasSeq!(now, siotaImpl!(to, now+1)); }
}

/** Same as $(D iota) but with explicit conversion to type $(D T).
    See also: http://forum.dlang.org/thread/mailman.955.1444358510.22025.digitalmars-d@puremagic.com?page=1
*/
auto iotaOf(T, B, E, S)(B begin = T.min,
                        E end = T.max,
                        S step = 1)
{
    import std.range : iota;
    import std.algorithm.iteration : map;
    import std.conv : to;
    return iota(begin, end, step).map!(a => cast(T)a);
}

// @safe pure unittest
// {
//     import std.array : array;
//     import std.exception : assertThrown;
//     import std.meta : AliasSeq;
//     foreach (T; AliasSeq!(ubyte, ushort, uint, ulong))
//     {
//         auto x = iotaOf!T(0, T.max + 1);
//         import traits_ex : ElementTypeOf;
//         static assert(is(ElementTypeOf!(x) == T));
//     }
// }

auto iotaOfExceptional(T, B, E, S)(B begin = T.min, E end = T.max, S step = 1)
{
    import std.range : iota;
    import std.algorithm.iteration : map;
    import std.conv : to;
    return iota(begin, end, step).map!(a => a.to!T);
}

// @safe pure unittest
// {
//     import std.array : array;
//     import std.exception : assertThrown;
//     import std.conv;
//     alias T = ubyte;
//     auto x = iotaOfExceptional!T(0, T.max + 1);
//     import traits_ex : ElementTypeOf;
//     static assert(is(ElementTypeOf!() == T));
//     assertThrown!ConvOverflowException(iotaOfExceptional!T(0, T.max + 1 + 1).array);
// }

/** Return Array of Key-Value Pairs of Associative Array $(D aa).
    See also: https://github.com/D-Programming-Language/druntime/pull/574
    See also: http://forum.dlang.org/thread/dxotcrutrlmszlidufcr@forum.dlang.org?page=2#post-fhkgitmifgnompkqiscd:40forum.dlang.org
*/
auto pairs(Key, Value)(Value[Key] aa)
{
    import std.typecons: Tuple, tuple;
    Tuple!(Key,Value)[] arr;
    arr.reserve(aa.length);
    foreach (key; aa.byKey)
    {
        arr ~= tuple(key, aa[key]);
    }
    return arr;
}
alias items = pairs; // TODO Is this Python-style naming better?

unittest
{
    string[int] x;
    x[0] = "a";
    import std.typecons : tuple;
    assert(x.pairs == [tuple(0, "a")]);
}

import std.traits: isInstanceOf;
import std.range: SortedRange;
import std.meta: allSatisfy, staticMap;
import std.typecons: Unqual;

template CommonElementType(Rs...)
{
    import std.traits: CommonType;
    import std.range: ElementType;
    alias CommonElementType = CommonType!(staticMap!(ElementType, Rs));
}
enum bool haveCommonElementType(Types...) = !is(CommonElementType!Types == void);
unittest
{
    static assert(haveCommonElementType!(bool[], int[]));
    static assert(!haveCommonElementType!(bool[], int[], string[]));
}

alias isSortedRange(R) = isInstanceOf!(SortedRange, R); // TODO Or use: __traits(isSame, TemplateOf!R, SortedRange)

/** Merge arguments with comparison.

    TODO is there a compacter (but as efficient) alternative to:

    foreach (i, R; Rs) { case i; ... }

    For higher type-safety we actually need a specific index type that only be
    either `undefined` or lie within range `[0 .. Rs.length - 1]`.

    See also: http://forum.dlang.org/thread/gvhqbkbkjbohnjawmvkl@forum.dlang.org#post-tnvzgcklcyutevogjsyi:40forum.dlang.org
*/
auto merge(alias less = "a < b", Rs...)(Rs rs) if (Rs.length > 1 &&
                                                   allSatisfy!(isSortedRange,
                                                               staticMap!(Unqual, Rs)) &&
                                                   haveCommonElementType!Rs)
{
    import std.range: isForwardRange, isBidirectionalRange, hasLength;

    alias E = CommonElementType!Rs;
    enum isBidirectional = allSatisfy!(isBidirectionalRange, staticMap!(Unqual, Rs));

    struct Result
    {
        this(Rs source)
        {
            this.source = source;
            this._lastFrontIndex = frontIndex;
            static if (isBidirectional)
                this._lastBackIndex = backIndex;
        }

        @property bool empty()
        {
            if (_lastFrontIndex == size_t.max)
                return true;
            static if (isBidirectional)
                return _lastBackIndex == size_t.max;
            else
                return false;
        }

        @property auto ref front()
        {
            import std.range: empty, front;
            final switch (_lastFrontIndex)
            {
                foreach (i, _; Rs)
                {
                    case i:
                        assert(!source[i].empty);
                        return source[i].front;
                }
            }
            assert(0);
        }

        private size_t frontIndex()
        {
            import std.range: empty, front;
            size_t bestIndex = size_t.max; // indicate undefined
            E bestElement;
            foreach (i, _; Rs)
            {
                import std.functional: binaryFun;
                if (!source[i].empty)
                {
                    if (bestIndex == size_t.max || // either this is the first or
                        binaryFun!less(source[i].front, bestElement))
                    {
                        bestIndex = i;
                        bestElement = source[i].front;
                    }
                }
            }
            return bestIndex;
        }

        void popFront()
        {
            import std.range: popFront;
            final switch (_lastFrontIndex)
            {
                foreach (i, _; Rs)
                {
                    case i:
                        source[i].popFront();
                        break;
                }
            }
            _lastFrontIndex = frontIndex;
        }

        static if (isBidirectional)
        {
            @property auto ref back()
            {
                import std.range: empty, back;
                final switch (_lastBackIndex)
                {
                    foreach (i, _; Rs)
                    {
                        case i:
                            assert(!source[i].empty);
                            return source[i].back;
                    }
                }
                assert(0);
            }

            private size_t backIndex()
            {
                import std.range: empty, back;
                import std.functional: binaryFun;
                size_t bestIndex = size_t.max; // indicate undefined
                E bestElement;
                foreach (i, _; Rs)
                {
                    if (!source[i].empty)
                    {
                        if (bestIndex == size_t.max || // either this is the first or
                            binaryFun!less(bestElement, source[i].back))
                        {
                            bestIndex = i;
                            bestElement = source[i].back;
                        }
                    }
                }
                return bestIndex;
            }

            void popBack()
            {
                import std.range: popBack;
                final switch (_lastBackIndex)
                {
                    foreach (i, _; Rs)
                    {
                        case i:
                            source[i].popBack();
                            break;
                    }
                }
                _lastBackIndex = backIndex;
            }
        }

        static if (allSatisfy!(isForwardRange, staticMap!(Unqual, Rs)))
        {
            @property auto save()
            {
                import std.range: save;
                Result result = this;
                foreach (i, _; Rs)
                {
                    result.source[i] = result.source[i].save;
                }
                return result;
            }
        }

        static if (allSatisfy!(hasLength, Rs))
        {
            @property size_t length()
            {
                size_t result;
                foreach (i, _; Rs)
                {
                    result += source[i].length;
                }
                return result;
            }

            alias opDollar = length;
        }

        public Rs source;
        private size_t _lastFrontIndex = size_t.max;
        static if (isBidirectional)
        {
            private size_t _lastBackIndex = size_t.max;
        }
    }

    return Result(rs);
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.range : assumeSorted, retro;

    int[] a = [ 1, 2, 3, 50, 60];
    double[] b = [ 10, 20, 30, 40 ];

    auto r = merge(a.assumeSorted,
                   b.assumeSorted);

    static assert(is(typeof(r.front) == double));
    assert(r.equal([1, 2, 3, 10, 20, 30, 40, 50, 60]));
    assert(r.retro.equal([60, 50, 40, 30, 20, 10, 3, 2, 1]));

    r.popFront();
    assert(r.equal([2, 3, 10, 20, 30, 40, 50, 60]));
    r.popBack();
    assert(r.equal([2, 3, 10, 20, 30, 40, 50]));
    r.popFront();
    assert(r.equal([3, 10, 20, 30, 40, 50]));
    r.popBack();
    assert(r.equal([3, 10, 20, 30, 40]));
    r.popFront();
    assert(r.equal([10, 20, 30, 40]));
    r.popBack();
    assert(r.equal([10, 20, 30]));
    r.popFront();
    assert(r.equal([20, 30]));
    r.popBack();
    assert(r.equal([20]));
    r.popFront();
    assert(r.empty);
}

/// See also: http://forum.dlang.org/post/gkdqakdogqevwzntpgtu@forum.dlang.org
template genTypeList(T, size_t n)
{
    import std.meta : AliasSeq;
    static if (n <= 1)
        alias genTypeList = T;
    else
        alias genTypeList = AliasSeq!(T, genTypeList!(T, n - 1));
}

/** Return Static Array $(D arr) as a $(D Tuple).
    See also: http://forum.dlang.org/post/gkdqakdogqevwzntpgtu@forum.dlang.org
    Check if std.conv.to() support conversion from T[n] to std.typecons.Tuple(T, ...).
*/
auto asTuple(T, size_t n)(ref T[n] arr)
{
    import std.typecons : Tuple;
    return Tuple!(genTypeList!(T, n))(arr);
}

/** Return: Adjacent $(D N)-Tuples of $(D r).
    TODO: Support ref return via $(D zip) for non-const case.
    TODO Use a ring buffer instead of copy?
    TODO Add a variant of adjacentTuples that return a static array instead?
    See also: http://forum.dlang.org/post/gkdqakdogqevwzntpgtu@forum.dlang.org
 */
auto adjacentTuples(size_t N, R)(R r)
    if (N >= 2 &&
        isInputRange!R)
{
    struct Range(R)
    {
        import std.typecons: Unqual, Tuple;
        alias E = Unqual!(ElementType!R);
        enum M = N - 1;  // temporary order
        alias P = E[M];
        alias T = Tuple!(genTypeList!(E, N)); // TODO functionize

        this(R r)
        {
            this._source = r;
            foreach (i; 0 .. M)
            {
                if (!empty)
                    popFront;
            }
        }

        static if (isInfinite!R)
        {
            enum bool empty = false;  // propagate infiniteness
        }
        else
        {
            bool empty() @property // TODO can't empty be const when R is a MapResult?
            {
                import std.range : empty;
                return _source.empty;
            }
        }

        auto ref front() @property
        {
            import std.range : front;
            T t;
            t[0 .. M] = _prevs.asTuple;
            t[M] = _source.front;
            return t;
        }

        void popFront()
        {
            static if (N >= 3)
            {
                // TODO use static foreach to do left-shifting

                // Need $(D copy) because $(D source) and $(D dest) may overlap.
                // See also: http://dlang.org/arrays.html#overlapping-copying
                import std.algorithm.mutation : copy;
                copy(_prevs[1 .. M], _prevs[0 .. M - 1]);
            }

            import std.range : front, popFront;
            _prevs[M - 1] = _source.front;
            _source.popFront();
        }

        private:
        P _prevs;
        R _source;
    }
    return Range!R(r);
}

auto adjacentPairs(R)(R r)
    if (isInputRange!R)
{
    return adjacentTuples!(2, R)(r);
}

auto adjacentTriples(R)(R r)
    if (isInputRange!R)
{
    return adjacentTuples!(3, R)(r);
}

///
unittest
{
    import std.typecons : t = tuple;
    import std.algorithm : equal, map;
    auto x = [1, 2, 3, 4, 5, 6, 7].map!(a => a); // test with ForwardRange
    auto y = x.adjacentTuples!4;
    assert(y.equal([t(1, 2, 3, 4),
                    t(2, 3, 4, 5),
                    t(3, 4, 5, 6),
                    t(4, 5, 6, 7)]));
}

///
unittest
{
    import std.typecons : t = tuple;
    import std.algorithm : equal;
    immutable x = [1, 2, 3, 4];
    auto y = x.adjacentPairs;
    assert(y.equal([t(1, 2), t(2, 3), t(3, 4)]));
}

///
unittest
{
    import std.typecons : t = tuple;
    import std.algorithm : equal;
    auto x = ["1", "2", "3", "4"];
    auto y = x.adjacentPairs;
    assert(y.equal([t("1", "2"), t("2", "3"), t("3", "4")]));
}

///
unittest
{
    import std.typecons : t = tuple;
    import std.algorithm : equal;
    immutable x = ["1", "2", "3", "4"];
    auto y = x.adjacentPairs;
    assert(y.equal([t("1", "2"), t("2", "3"), t("3", "4")]));
}

auto rangify(T)(T range)
    if (__traits(hasMember, T, "length") &&
        __traits(hasMember, T, "opIndex"))
{
    struct Range
    {
        bool empty() { return _counter == range.length; }
        auto front() { return range[_counter]; }
        auto popFront() { _counter++; }
        T range;
        ulong _counter;
    }
    return Range(range);
}

struct S
{
    int[] arr;
    auto length() { return arr.length; }
    int opIndex(size_t i) { return arr[i]; }
}

unittest
{
    import std.algorithm : equal;
    auto s = S();
    s.arr = [1, 2, 3];
    assert(s.rangify.equal([1, 2, 3]));
}

/** Overload has questionable memory safety.  Would be quite cool if DIP-1000
    could support this use case
    See also: http://forum.dlang.org/post/qgrbmkqxffgeiqaigdic@forum.dlang.org
*/
auto staticLengthRange(T, size_t n)(ref T[n] arr)
{
    return .staticLengthRange!(n, T[])(arr[]); // TODO DIP-1000 scope
}

import std.range.primitives : hasLength, isInputRange;

auto staticLengthRange(size_t n, R)(R range)
    if (isInputRange!R && hasLength!R)
{
    struct Result
    {
        enum size_t length = n;
        R _range;
        alias _range this;
    }
    assert (range.length == n);
    return Result(range);
}


unittest
{
    import std.algorithm.iteration : map;

    int[3] sarr = [1, 2, 3];
    auto r1 = sarr.staticLengthRange;
    static assert (isInputRange!(typeof(r1)));
    static assert (r1.length == 3);

    auto arr = [1, 2, 3, 4];
    auto r2 = arr.map!(a => a * 2).staticLengthRange!4;
    static assert (r2.length == 4);
}
