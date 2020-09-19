class Base {}
class Derived : Base {}

// classes
@safe pure unittest
{
    Base bd = new Derived();              // implicit conversion
    Derived db = cast(Derived)new Base(); // explicit conversion
}

// dynamic array of `const` or `immutable` class elements
@safe pure unittest
{
    const(Base)[] ca = (const(Derived)[]).init;
    immutable(Base)[] ia = (immutable(Derived)[]).init;
}

// static array of mutable, `const` or `immutable` class elements
@safe pure unittest
{
    const(Base)[3] ca = (const(Derived)[3]).init;
    immutable(Base)[3] ia = (immutable(Derived)[3]).init;
    Base[3] ma = (Derived[3]).init;
}
