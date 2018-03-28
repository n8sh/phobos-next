/** Stack.
    See_Also: http://forum.dlang.org/thread/wswbtzakdvpgaebuhbom@forum.dlang.org
*/

struct Stack(T)
{
    import std.array: Appender, appender;

    Appender!(T[]) _app;

    @property ref inout(T) top() inout { return _app.data[$ - 1]; };

    @property bool empty() const { return _app.data.length == 0; }

    void pop() { _app.shrinkTo(_app.data.length - 1); }

    T backPop() { T value = top; _app.shrinkTo(_app.data.length - 1); return value; }

    void push(T t) { _app.put(t); }
}

@safe pure unittest
{
    alias T = uint;

    Stack!T s;
    assert(s.empty);

    // pushBack:

    s.push(13);
    assert(!s.empty);
    assert(s.top == 13);

    s.push(14);
    assert(!s.empty);
    assert(s.top == 14);

    s.push(15);
    assert(!s.empty);
    assert(s.top == 15);

    // pop:

    s.pop();
    assert(!s.empty);
    assert(s.top == 14);

    s.pop();
    assert(!s.empty);
    assert(s.top == 13);

    s.pop();
    assert(s.empty);

    // push:

    s.push(13);
    s.push(14);
    s.push(15);
    assert(!s.empty);
    assert(s.top == 15);

    // backPop:

    assert(s.backPop() == 15);
    assert(s.backPop() == 14);
    assert(s.backPop() == 13);

    assert(s.empty);
}
