module moval;

/** Is `true` if `A` is an l-value, `false` otherwise.
    See also: https://forum.dlang.org/post/mailman.4192.1454351296.22025.digitalmars-d-learn@puremagic.com
    TODO Add to Phobos
*/
enum isLvalue(alias A) = is(typeof((ref _){}(A)));

/** Is `true` if `A` is an l-value, `false` otherwise.
 */
enum isRvalue(alias A) = !isLvalue!A;

unittest
{
    int i;
    string s;
    static assert(isLvalue!i);
    static assert(isLvalue!s);
    static assert(!isLvalue!13);
    static assert(!isLvalue!"a");
}

/** Move contents of `e` into an r-value and return it.
    See also: http://forum.dlang.org/post/lxfwyxtgfosdtsdjwgwz@forum.dlang.org
*/
E movedToRvalue(E)(ref E e)
{
    import std.algorithm.mutation : move;
    E value;
    move(e, value);             // this can be optimized
    return value;
}

@safe pure nothrow @nogc:

struct S
{
    @disable this(this);
    int x;
}

void consume(S)
{
}

unittest
{
    auto s = S(13);
    static assert(!__traits(compiles, { consume(s); }));
    static assert(__traits(compiles, { consume(S(14)); })); // TODO optimize
    consume(s.movedToRvalue());
}
