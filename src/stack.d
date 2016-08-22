/** Stack.

    See also: http://forum.dlang.org/thread/wswbtzakdvpgaebuhbom@forum.dlang.org
*/
struct Stack(T)
{
    import std.array: Appender, appender;

    ref inout(T) top() inout { return _app.data[$ - 1]; };

    bool empty() const { return _app.data.length == 0; }

    /** Push element `t`. */
    void push(in T t) { _app.put(t); }

    /** Pop element. */
    void pop()
    {
        assert(!empty);
        _app.shrinkTo(_app.data.length - 1);
    }

    private Appender!(T[]) _app; // TODO replace with array_ex.d
    alias _app this;
}
