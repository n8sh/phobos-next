module test_class_hash;

import core.internal.hash : hashOf;

/** Hash that distinguishes `Expr(X)` from `NounExpr(X)`.
 *
 * See_Also: https://forum.dlang.org/post/lxqoknwuujbymolnlyfw@forum.dlang.org
 */
hash_t hashOfPolymorphic(Class)(Class aClassInstance) @trusted pure nothrow @nogc
if (is(Class == class))
{
    assert(Class.alignof == 8);
    return (cast(hash_t)(cast(void*)typeid(Class)) >> 3) ^ hashOf(aClassInstance);
}

version(unittest)
{
    private static:

    class Thing
    {
        @property override bool opEquals(const scope Object that) const @safe pure nothrow @nogc
        {
            if (typeid(this) !is typeid(that)) { return false; }
            assert(0);
        }
    }

    class Expr : Thing
    {
        @safe pure nothrow @nogc:
        alias Data = string;
        this(Data data)
        {
            this.data = data;
        }
        @property override bool opEquals(const scope Object that) const @safe pure nothrow @nogc
        {
            if (typeid(this) !is typeid(that)) { return false; }
            return data == (cast(typeof(this))(that)).data;
        }
        @property override hash_t toHash() const @safe pure nothrow @nogc
        {
            return hashOf(data);
        }
        Data data;
    }

    class NounExpr : Expr
    {
        @safe pure nothrow @nogc:
        this(Data data)
        {
            super(data);
        }
        @property override hash_t toHash() const @safe pure nothrow @nogc
        {
            return hashOf(data);
        }
    }

    class Year : Thing
    {
        @safe pure nothrow @nogc:
        alias Data = long;
        @property override hash_t toHash() const @safe pure nothrow @nogc
        {
            return hashOf(data);
        }
        Data data;
    }
}

@safe pure nothrow unittest
{
    auto car1 = new Expr("car");
    auto car2 = new Expr("car");
    auto bar1 = new Expr("bar");
    auto ncar = new NounExpr("car");

    void testEqual() @safe pure nothrow @nogc
    {
        assert(car1.opEquals(car2));
        assert(!car1.opEquals(bar1));
        assert(!car2.opEquals(bar1));
        // TODO assert(car1 == car2);
        assert(hashOf(car1) == hashOf(car2));
        assert(hashOfPolymorphic(car1) == hashOfPolymorphic(car2));
    }

    void testDifferent1() @safe pure nothrow @nogc
    {
        assert(!car1.opEquals(bar1));
        // TODO assert(car1 != bar1);
        assert(hashOf(car1) != hashOf(bar1));
        assert(hashOfPolymorphic(car1) != hashOfPolymorphic(bar1));
    }

    void testDifferent2() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) == hashOf(ncar));
        assert(hashOfPolymorphic(car1) != hashOfPolymorphic(ncar));
    }

    testEqual();
    testDifferent1();
    testDifferent2();
}
