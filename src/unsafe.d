module unsafe;

/** Call the possibly unsafe function `fn` in a @trusted way.
 *
 * See_Also: https://forum.dlang.org/post/amvspqyavdavzgjegkzt@forum.dlang.org
 *
 * TODO Add to std.meta or std.typecons.
 */
template unsafe(alias fn)
{
    @trusted auto unsafe(T...)(T args)
    {
        return fn(args);
    }
}

@system void dummy(int n) {}

//
@safe unittest
{
    unsafe!({ dummy(2); });
    unsafe!dummy(2);
}
