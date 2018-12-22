module test_class_hash;

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
    Data data;
}

class Year : Thing
{
    @safe pure nothrow @nogc:
    alias Data = long;
    Data data;
}

@safe pure unittest
{
    auto expr = new Expr("car");
}
