/** Stack.

    See also: http://forum.dlang.org/thread/wswbtzakdvpgaebuhbom@forum.dlang.org
*/
struct Stack(T)
{
    import std.array: Appender, appender;

    ref inout(T) top() inout { return _app.data[$ - 1]; };
    alias back = top;

    bool empty() const { return _app.data.length == 0; }

    /** Push element `t`. */
    void push(in T t) { _app.put(t); }

    /** Pop element. */
    void pop()
    {
        assert(!empty);
        try
        {
            _app.shrinkTo(_app.data.length - 1);
        }
        catch (Exception e)
        {
            assert(false, "Assertion was thrown");
        }
    }

    private Appender!(T[]) _app; // TODO replace with array_ex.d
    alias _app this;
}

@safe pure nothrow /* TODO @nogc */ unittest
{
    alias T = int;

    Stack!T s;
    assert(s.empty);

    s.push(13);
    assert(!s.empty);
    assert(s.top == 13);

    s.pop();
    assert(s.empty);
}
