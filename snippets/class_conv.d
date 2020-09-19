class Base {}
class Derived : Base {}

// classes
@safe pure unittest
{
    Base _bd = new Derived();              // implicit conversion
    Derived _db = cast(Derived)new Base(); // explicit conversion
}

// dynamic array of `const` or `immutable` class elements
@safe pure unittest
{
    const(Base)[] _ca = (const(Derived)[]).init;
    immutable(Base)[] _ia = (immutable(Derived)[]).init;
}

// static array of mutable, `const` or `immutable` class elements
@safe pure unittest
{
    const(Base)[3] _ca = (const(Derived)[3]).init;
    immutable(Base)[3] _ia = (immutable(Derived)[3]).init;
    Base[3] _ma = (Derived[3]).init;
}
