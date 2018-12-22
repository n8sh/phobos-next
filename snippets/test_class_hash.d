module test_class_hash;

import core.internal.hash : hashOf;

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

@safe pure nothrow unittest
{
    import dbgio;

    auto car1 = new Expr("car");
    auto car2 = new Expr("car");
    auto bar1 = new Expr("bar");
    auto ncar = new NounExpr("car");

    void testEqual() @nogc
    {
        assert(hashOf(car1) == hashOf(car2));
    }

    void testDifferent1() @nogc
    {
        assert(hashOf(car1) != hashOf(bar1));
    }

    void testDifferent2() @nogc
    {
        assert(hashOf(car1) != hashOf(ncar));
    }

    dln("car1: ", hashOf(car1));
    dln("car2: ", hashOf(car2));
    dln("bar1: ", hashOf(bar1));
    dln("ncar: ", hashOf(ncar));

    testEqual();
    testDifferent1();
    testDifferent2();
}
