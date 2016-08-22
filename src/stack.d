/** Stack.

    See also: http://forum.dlang.org/thread/wswbtzakdvpgaebuhbom@forum.dlang.org
*/
struct Stack(T)
{
    import array_ex;

    ref inout(T) back() inout { return _app[$ - 1]; };
    alias top = back;

    /** Push element `t`. */
    void pushBack(in T t) @safe { _app.pushBack(t); }

    /** Pop element. */
    void popBack()
    {
        assert(!empty);
        try
        {
            _app.popBack;
        }
        catch (Exception e)
        {
            assert(false, "Assertion was thrown");
        }
    }

    private Array!(T, Ordering.unsorted, false) _app;
    alias _app this;
}

@safe pure nothrow /* TODO @nogc */ unittest
{
    alias T = int;

    Stack!T s;
    assert(s.empty);

    s.pushBack(13);
    assert(!s.empty);
    assert(s.back == 13);

    s.pushBack(14);
    assert(!s.empty);
    assert(s.back == 14);

    s.pushBack(15);
    assert(!s.empty);
    assert(s.back == 15);

    s.popBack();
    assert(!s.empty);
    assert(s.back == 14);

    s.popBack();
    assert(!s.empty);
    assert(s.back == 13);

    s.popBack();
    assert(s.empty);
}
