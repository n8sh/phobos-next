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
