module expected;

@safe pure:

/** Expected sum type of either a `Result` or an `Error`.
 *
 * See_Also: https://www.youtube.com/watch?v=nVzgkepAg5Y
 */
struct Expected(Result, Error)
{
    @safe:

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

    void opAssign(Result result) @trusted
    {
        clear();
        _result = result;
    }

    private void clear() @trusted
    {
        release();
        import std.traits : hasElaborateCopyConstructor;
        static if (hasElaborateCopyConstructor!Result)
        {
        }
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
    void popFront()
    {
        assert(!empty);
        clear();
    }

private:
    union
    {
        Result _result;
        Error _error;           // TODO wrap in `Unexpected`
    }
    bool _hasResult = false; // TODO remove when `_result` and ` _error` can store this state
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
