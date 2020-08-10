import std;

@safe unittest
{
    // prevent message: cannot implicitly convert expression `uniform(0, 2)` of type `int` to `bool` (d-dmd)
    bool x = cast(bool)uniform(0, 2);
}
