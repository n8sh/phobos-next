/** Stack.

    See also: http://forum.dlang.org/thread/wswbtzakdvpgaebuhbom@forum.dlang.org
*/
struct Stack(T)
{
    import array_ex;

    ref inout(T) back() inout { return _store[$ - 1]; };
    alias top = back;

    /** Push element `t` to stack. */
    void pushBack(in T t) @safe { _store.pushBack(t); }

    /** Pop element from stack. */
    void popBack()
    {
        assert(!empty);
        _store.popBack;
    }

    /** Pop back element and return it. */
    T backPop()
    {
        assert(!empty);
        T value = back;
        _store.popBack;
        return value;
    }

    /** Returns: `true` if stack is empty `false` otherwise. */
    bool empty() const @safe pure nothrow @nogc { return _store.empty; }

private:
    Array!(T, Ordering.unsorted, false) _store;
}

@safe pure nothrow /* TODO @nogc */ unittest
{
    alias T = int;

    Stack!T s;
    assert(s.empty);

    // test pushBack:

    s.pushBack(13);
    assert(!s.empty);
    assert(s.back == 13);

    s.pushBack(14);
    assert(!s.empty);
    assert(s.back == 14);

    s.pushBack(15);
    assert(!s.empty);
    assert(s.back == 15);

    // test popBack:

    s.popBack();
    assert(!s.empty);
    assert(s.back == 14);

    s.popBack();
    assert(!s.empty);
    assert(s.back == 13);

    s.popBack();
    assert(s.empty);

    // test pushBack:

    s.pushBack(13);
    assert(!s.empty);
    assert(s.back == 13);

    s.pushBack(14);
    assert(!s.empty);
    assert(s.back == 14);

    s.pushBack(15);
    assert(!s.empty);
    assert(s.back == 15);

    // test backPop:

    assert(s.backPop() == 15);
    assert(s.backPop() == 14);
    assert(s.backPop() == 13);

    assert(s.empty);
}
