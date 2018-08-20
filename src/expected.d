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
        _hasResult = true;
    }

    /** Is `true` iff this has a result of type `Result`. */
    bool hasResult() const { return _hasResult; }

    void orElse(alias elseWork)() const
    {
    }

    // range interface:
    @property bool empty() const
    {
        return !hasResult;
    }

    @property Result front() const @trusted
    {
        assert(!empty);
        return _result;
    }

    @property void popFront() @trusted
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

    bool _hasResult = false;
}

@safe pure nothrow @nogc unittest
{
    auto x = Expected!(string, int)("alpha");
    assert(x.hasResult);
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
