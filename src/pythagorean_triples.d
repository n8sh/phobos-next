/** Pythogorean triple generators.
 *
 * See_Also: https://forum.dlang.org/post/q08qsm$22j3$1@digitalmars.com
 */
module pythogorean_triples;

/// Pythogorean triple generator rangeg.
struct PossiblePythagoreanTriples(T)
{
    /// Pythogorean triple.
    struct Triple
    {
        T x, y, z;
        version(none)
        @property void toString(scope void delegate(const(char)[]) @safe sink) const @safe
        {
            import std.conv : to;
            sink(x.to!string);
            sink(",");
            sink(y.to!string);
            sink(",");
            sink(z.to!string);
        }
    }

    @property Triple front() const
    {
        return _store;
    }

    void nextTriple()
    {
        if (++_store.y == _store.z)
        {
            if (++_store.x == _store.z)
            {
                ++_store.z;     // if `_store.z` becomes 0 empty should be true
                _store.x = 1;
            }
            _store.y = _store.x;
        }
    }

    void popFront()
    {
        do
        {
            nextTriple();
        } while (_store.x*_store.x + _store.y*_store.y != _store.z*_store.z);
    }

    enum empty = false;

    private Triple _store = Triple(1, 1, 2);
}

/// Get all Pythogorean triples in an infinite generator.
auto pythagoreanTriples(T = size_t)()
{
    return PossiblePythagoreanTriples!T();
}

///
@safe pure nothrow @nogc unittest
{
    auto t = pythagoreanTriples!size_t;
    alias Triple = typeof(t.front);
    assert(t.front == Triple(1,1,2)); t.popFront();
    assert(t.front == Triple(3,4,5)); t.popFront();
    assert(t.front == Triple(6,8,10)); t.popFront();
    assert(t.front == Triple(5,12,13)); t.popFront();
    assert(t.front == Triple(9,12,15)); t.popFront();
    assert(t.front == Triple(8,15,17)); t.popFront();
    assert(t.front == Triple(12,16,20)); t.popFront();
}
