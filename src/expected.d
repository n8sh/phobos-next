module expected;

@safe pure:

/** Expected sum type of either a `Result` or an `Error`.
 *
 * See_Also: https://www.youtube.com/watch?v=nVzgkepAg5Y
 */
struct Expected(Result, Error)
{
    this(Result result) @trusted
    {
        _result = result;
        _hasResult = true;
    }

    this(Error error) @trusted
    {
        _error = error;
        _hasResult = false;
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
        assert(!empty);
        return _result;
    }
    void popFront() @trusted
    {
        assert(!empty);
        destroy(_result);
        if (isAddress!Expected)
        {
            _result = null;
        }
        _hasResult = false;
    }

private:
    union
    {
        Result _result;
        Error _error;
    }

    bool _hasResult = false; // TODO remove when `_result` and ` _error` can store this state
}

@safe pure nothrow @nogc unittest
{
    auto x = Expected!(string, int)("alpha");
    assert(x.hasResult);
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
