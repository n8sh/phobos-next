module rational;

Rational!(I1) rational(I1, I2)(I1 , I2)
{
    return typeof(return)();
}

struct Rational(Int)
{
    // bool opEquals(Rhs)(Rhs _) const { return true; }
    bool opEquals(Rhs)(Rhs _) { return true; }
}

@nogc unittest
{
    auto _ = rational(1, 2);
}
