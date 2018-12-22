module test_class_hash;

import core.internal.hash : hashOf;
import dbgio : dln;

class Thing
{
}

class Expr : Thing
{
    @safe pure nothrow @nogc:
    alias Data = string;
    this(Data data)
    {
        this.data = data;
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

/** Hash that distinguishes `Expr(X)` from `NounExpr(X)`. */
hash_t hashOfPolymorphic(Class)(Class someThing) @trusted pure nothrow @nogc
if (is(Class == class))
{
    assert(Class.alignof == 8);
    dln(cast(void*)typeid(Class));
    return cast(hash_t)(cast(void*)typeid(Class)) ^ hashOf(someThing);
}

@safe pure nothrow unittest
{
    auto car1 = new Expr("car");
    auto car2 = new Expr("car");
    auto bar1 = new Expr("bar");
    auto ncar = new NounExpr("car");

    void testEqual() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) == hashOf(car2));
    }

    void testDifferent1() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) != hashOf(bar1));
    }

    void testDifferent2() @safe pure nothrow @nogc
    {
        assert(hashOf(car1) != hashOf(ncar));
    }

    dln("car1: ", hashOf(car1), " ", hashOfPolymorphic(car1));
    dln("car2: ", hashOf(car2), " ", hashOfPolymorphic(car2));
    dln("bar1: ", hashOf(bar1), " ", hashOfPolymorphic(bar1));
    dln("ncar: ", hashOf(ncar), " ", hashOfPolymorphic(ncar));

    testEqual();
    testDifferent1();
    testDifferent2();
}
