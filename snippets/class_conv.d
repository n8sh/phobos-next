class Base {}
class Derived : Base {}

@safe pure unittest
{
    Base bd = new Derived();              // implicit conversion
    Derived db = cast(Derived)new Base(); // explicit conversion
}

@safe pure unittest
{
    const(Base)[] ba = (const(Derived)[]).init;
}

@safe pure unittest
{
    immutable(Base)[] ba = (immutable(Derived)[]).init;
}

@safe pure unittest
{
    const(Derived)[3] da;
    const(Base)[3] ba = da;
}
