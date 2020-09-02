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
    int x;
    alias x this;
}

struct S
{
    this(E e)
    {
        this.e = e;             // TODO: last ref of `e` is moved
    }

    this(E e)
    {
        this.e = e;             // TODO: last ref of `e` is moved
    }

    E I1(E e)
    {
        import core.lifetime : move;
        return move(e);
    }

    E I2(E e)
    {
        return e;               // TODO: last ref of `e` is moved
    }

    E identity(const E e)
    {
        return e;               // TODO: last ref of `e` is moved
    }

    E f(E e)
    {
        return identity(e);     // TODO: last ref of `e` is moved
    }

    E e;
}
