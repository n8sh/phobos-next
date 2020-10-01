struct Z
{
    // Z opBinary(string s)(auto ref const Z rhs) const @trusted // direct value
    // {
    //     return typeof(return)(this.x + rhs.x);
    // }
    int x;
}

Z opBinary(string s)(auto ref const Z lhs,
                     auto ref const Z rhs)
{
}

@safe pure unittest
{
	Z a, b;
    auto c = a + b;
}
