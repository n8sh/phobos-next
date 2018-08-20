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

    this(Result result) @trusted
    {
        // TODO reuse opAssign?
        _result = result;       // TODO use moveEmplace here aswell?
        _hasResult = true;
    }

    this(Error error) @trusted
    {
        // TODO reuse opAssign?
        _error = error;         // TODO use moveEmplace here aswell?
        _hasResult = false;
    }

    void opAssign(Result result) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(result, _result);
        _hasResult = true;
    }

    void opAssign(Error error) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(error, _error);
        _hasResult = false;
    }

    private void clear() @trusted
    {
        release();
        static if (isAddress!Result)
        {
            _result = null;
        }
    }

    private void release() @trusted
    {
        if (hasResult)
        {
            destroy(_result);
            _hasResult = false;
        }
        else
        {
            destroy(_error);
            // TODO change _hasResult?
        }
    }

    /** Is `true` iff this has a result of type `Result`. */
    bool hasResult() const { return _hasResult; }

    void orElse(alias elseWork)() const
    {
        // TODO
    }

    // range interface:
    @property bool empty() const
    {
        return !hasResult;
    }
    @property inout(Result) front() inout @trusted
    {
        assert(hasResult);
        return _result;
    }
    void popFront()
    {
        assert(!empty);
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

@safe pure nothrow @nogc unittest
{
    alias E = Expected!(string, int);

    auto x = E("alpha");
    assert(x.hasResult);
    assert(!x.empty);

    x.popFront();
    assert(!x.hasResult);
    assert(x.empty);
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
