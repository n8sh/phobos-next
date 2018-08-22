/** Wrapper type for a sum-type (union) of an unexpected and expected value.
 *
 * See_Also: https://github.com/dlang/phobos/pull/6665
 *
 * TODO https://dlang.org/phobos/std_typecons.html#.apply
 *
 * TODO later on: remove _hasResult when `_result` and ` _error` can store this state
 * "collectively" for instance when both are pointers or classes (use trait
 * `isAddress`)
 */
module expected;

@safe pure:

/** Expected sum type of either a `T` or an `U`.
 *
 * See_Also: https://www.youtube.com/watch?v=nVzgkepAg5Y
 */
struct Expected(T, U)
{
    @safe:

    // TODO ok for default construction to initialize
    // - _result = T.init (zeros)
    // - _hasResult = true (better to have _isError so default is zero bits here aswell?)

    /// Construct from result `result.`
    this(T result) @trusted
    {
        // TODO reuse opAssign?
        _result = result;       // TODO use moveEmplace here aswell?
        _hasResult = true;
    }

    /// Construct from error `error.`
    this(Unexpected!U error) @trusted
    {
        // TODO reuse opAssign?
        _error = error; // TODO use moveEmplace here aswell?
        _hasResult = false;
    }

    /// Assign from result `result.`
    void opAssign(T result) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(result, _result);
        _hasResult = true;
    }

    /// Assign from error `error.`
    void opAssign(U error) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(error, _error);
        _hasResult = false;
    }

    /// Clear (empty) contents.
    private void clear() @trusted
    {
        release();
        static if (isAddress!T)
        {
            _result = null;
        }
    }

    /// Release any memory used to store contents.
    private void release() @trusted
    {
        import std.traits : hasElaborateDestructor;
        if (hasResult)
        {
            static if (!is(T == class))
            {
                static if (hasElaborateDestructor!T)
                {
                    destroy(_result);
                }
            }
            _hasResult = false;
        }
        else
        {
            static if (!is(U == class))
            {
                static if (hasElaborateDestructor!U)
                {
                    destroy(_error);
                }
            }
            destroy(_error);
            // TODO change _hasResult?
        }
    }

    /** Is `true` iff this has a result of type `T`. */
    bool hasResult() const { return _hasResult; }

    import std.traits : CommonType;

    /** Get current value if any or call function `elseWorkFun` with compatible return value.
     *
     * TODO is this anywhere near what we want?
     */
    CommonType!(T, typeof(elseWorkFun())) orElse(alias elseWorkFun)() const
    if (is(CommonType!(T, typeof(elseWorkFun()))))
    {
        if (hasResult)
        {
            return result;
        }
        else
        {
            return elseWorkFun(); // TODO is this correct
        }
    }

    // range interface:

    /// Check if empty.
    @property bool empty() const
    {
        return !_hasResult;
    }

    /// Get current value.
    @property inout(T) front() inout @trusted
    {
        assert(_hasResult);
        return _result;
    }

    /// Pop (clear) current value.
    void popFront()
    {
        assert(_hasResult);
        clear();
    }

private:
    union
    {
        T _result;         // TODO do we need to default-initialize this somehow?
        Unexpected!U _error;           // TODO wrap in `Unexpected`
    }
    bool _hasResult = true;     // @andralex: ok to opportunistic and default to `T.init`
}

private struct Unexpected(U)
{
    U value;
    alias value this;
}

auto expected(T, U)(auto ref T value)
{
    return Expected!(T, U)(value);
}

auto unexpected(T, U)(auto ref U error)
{
    return Expected!(T, U)(Unexpected!U(error));
}

@safe pure nothrow @nogc unittest
{
    alias T = string;           // expected type
    alias U = int;
    alias E = Expected!(T, int);

    auto x = E("alpha");
    assert(x.hasResult);
    assert(!x.empty);

    x.popFront();
    assert(!x.hasResult);
    assert(x.empty);

    import std.typecons : Nullable;

    auto e = E(Unexpected!int(int.init));
    assert(!e.hasResult);
    assert(x.empty);
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
