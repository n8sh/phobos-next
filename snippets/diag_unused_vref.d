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

struct S
{
    this(E e)
    {
        this.e = e;               // TODO: last ref of `e` is moved
        assert(e.copyCount == 0); // TODO: no move
    }

    E I1(E e)
    {
        import core.lifetime : move;
        return move(e);         // already move
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
