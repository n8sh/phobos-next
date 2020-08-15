class Base
{
    abstract void f();
}
class Derived : Base
{
}

unittest
{
    scope b = new Base();
    scope d = new Derived();
}
