module expected;

@safe pure:

/** Expected type.
 *
 * Returns:
 */
struct Expected(Result, Error)
{
    this(Result result) @trusted
    {
        _result = result;
        _ok = true;
    }

    this(Error error) @trusted
    {
        _error = error;
        _ok = true;
    }

    bool isOk() const { return _ok; }

    void orElse(alias elseWork)()
    {
    }

private:
    union
    {
        Result _result;
        Error _error;
    }

    bool _ok = false;
}

@safe pure nothrow @nogc unittest
{
    auto x = Expected!(string, int)("alpha");
    assert(x.isOk);
}
