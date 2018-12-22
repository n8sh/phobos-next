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

unittest
{
    import dbgio;
    auto car = new Expr("car");
    auto car2 = new Expr("car");
    assert(hashOf(car) == hashOf(car2));
    dln(hashOf(car));
    dln(hashOf(car2));

    auto bar = new Expr("bar");
    dln(hashOf(bar));

    auto ncar = new NounExpr("car");
    dln(hashOf(ncar));
}
