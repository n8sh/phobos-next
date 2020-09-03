// REQUIRED_ARGS: -w -vcolumns -unittest -diagnose=unused

/*
TEST_OUTPUT:
---
fail_compilation/diag_unused_extern.d(18,13): Warning: unused private variable `x1` of module `diag_unused_extern`
---
*/

struct E
{
    this(this)
    {
        copyCount += 1;
    }
    int x;
    alias x this;
    uint copyCount;
}

// Can move e because all refs to `e` are direct returns.
E yes_move_return_e(E e)
{
    if (true)
        return e;               // TODO: last ref of `e` is moved
    else
        return e;               // TODO: last ref of `e` is moved
}

E no_move_return_e_yes_move_return_f(E e)
{
    auto f = e;                 // can't move `e`
    if (e.x == 0)
        return e;               // can't move `e`
    else if (e.x == 1)
        return e;               // can't move `e`
    else
        return f;               // can move `f`
}

E can_move_assign_from_e_and_return_f(E e)
{
    auto f = e;                 // can move `e`
    return f;                   // can move `f`
}

struct S
{
    this(E e)
    {
        this.e = e; // last ref so `e` can be moved
    }
    E e;
}
