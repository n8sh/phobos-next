module expected;

@safe pure:

/** Expected sum type of either a `Result` or an `Error`.
 *
 * See_Also: https://www.youtube.com/watch?v=nVzgkepAg5Y
 */
struct Expected(Result, Error)
{
    @safe:

    // TODO ok for default construction to initialize
    // - _result = Result.init (zeros)
    // - _hasResult = true (better to have _isError so default is zero bits here aswell?)

    /// Construct from result `result.`
    this(Result result) @trusted
    {
        // TODO reuse opAssign?
        _result = result;       // TODO use moveEmplace here aswell?
        _hasResult = true;
    }

    /// Construct from error `error.`
    this(Error error) @trusted
    {
        // TODO reuse opAssign?
        _error = error;         // TODO use moveEmplace here aswell?
        _hasResult = false;
    }

    /// Assign from result `result.`
    void opAssign(Result result) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(result, _result);
        _hasResult = true;
    }

    /// Assign from error `error.`
    void opAssign(Error error) @trusted
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
        static if (isAddress!Result)
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
            static if (!is(Result == class))
            {
                static if (hasElaborateDestructor!Result)
                {
                    destroy(_result);
                }
            }
            _hasResult = false;
        }
        else
        {
            static if (!is(Error == class))
            {
                static if (hasElaborateDestructor!Error)
                {
                    destroy(_error);
                }
            }
            destroy(_error);
            // TODO change _hasResult?
        }
    }

    /** Is `true` iff this has a result of type `Result`. */
    bool hasResult() const { return _hasResult; }

    /// Get current value if any or call function `elseWorkFun` with compatible return value.
    CommonType!(Result,
                typeof(elseWorkFun()))
    orElse(alias elseWorkFun)() const
        if (is(CommonType!(Result,
                           typeof(elseWorkFun()))))
    {
        if (hasResult)
        {
            return result;
        }
        else
        {
            return elseWorkFun();
        }
        // TODO
    }

    // range interface:

    /// Check if empty.
    @property bool empty() const
    {
        return !_hasResult;
    }

    /// Get current value.
    @property inout(Result) front() inout @trusted
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
        Result _result;         // TODO do we need to default-initialize this somehow?
        Error _error;           // TODO wrap in `Unexpected`
    }
    // TODO special case and remove when `_result` and ` _error` can store this
    // state
    bool _hasResult = true;     // @andralex says ok to default Result.init by default
}

private struct Unexpected(T)
{
    T value;
    alias value this;
}

auto expected(Result, Error)(auto ref Result value)
{
    return Expected!(Result, Error)(value);
}

auto unexpected(Result, Error)(auto ref Error error)
{
    return Expected!(Result, Error)(Unexpected!Error(error));
}

@safe pure nothrow @nogc unittest
{
    alias Result = string;
    alias Error = int;
    alias E = Expected!(Result, Error);

    auto x = E("alpha");
    assert(x.hasResult);
    assert(!x.empty);

    x.popFront();
    assert(!x.hasResult);
    assert(x.empty);

    auto e = E(Error.init);
    assert(!e.hasResult);
    assert(x.empty);
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
