// REQUIRED_ARGS: -w -vcolumns -unittest -diagnose=unused

/*
TEST_OUTPUT:
---
fail_compilation/diag_unused_extern.d(18,13): Warning: unused private variable `x1` of module `diag_unused_extern`
---
*/

struct E
{
    @disable this(this);
    // this(this) { copyCount += 1; }
    int x;
    alias x this;
    uint copyCount;
}

// Can move e because all refs to `e` are direct returns.
E yes_move_one_return_e(E e)
{
    return e;                   // single ref of `e` is moved
}

// Can move e because all refs to `e` are direct returns.
E yes_move_two_return_e(E e)
{
    if (true)
        return e;               // first ref of `e` is moved
    else
        return e;               // second ref of `e` is moved
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
