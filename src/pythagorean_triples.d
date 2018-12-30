/** Pythogorean triple generators.
 *
 * See_Also: https://forum.dlang.org/post/q08qsm$22j3$1@digitalmars.com
 */
module pythogorean_triples;

import std.algorithm;

/// Pythogorean triple generator rangeg.
struct PossiblePythagoreanTriples(T)
{
    /// Pythogorean triple.
    struct Triple
    {
        T x, y, z;
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
    Triple triple = Triple(1, 1, 2);

    Triple front()
    {
        return triple;
    }

    void popFront()
    {
        if (++triple.y == triple.z)
        {
            if (++triple.x == triple.z)
            {
                ++triple.z;     // if `triple.z` becomes 0 empty should be true
                triple.x = 1;
            }
            triple.y = triple.x;
        }
    }

    enum empty = false;
}

/// Get all Pythogorean triples in an infinite generator.
auto pythagoreanTriples(T = size_t)()
{
    return PossiblePythagoreanTriples!T().filter!(p => p.x*p.x + p.y*p.y == p.z*p.z);
}

@safe pure @nogc unittest
{
    auto t = pythagoreanTriples!size_t;
    alias Triple = typeof(t.front);
    assert(t.front == Triple(3,4,5)); t.popFront();
    assert(t.front == Triple(6,8,10)); t.popFront();
    assert(t.front == Triple(5,12,13)); t.popFront();
    assert(t.front == Triple(9,12,15)); t.popFront();
    assert(t.front == Triple(8,15,17)); t.popFront();
    assert(t.front == Triple(12,16,20)); t.popFront();
}
