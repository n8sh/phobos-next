/** Check presence of proposed extensions/modifications to the D language itself.
 *
 * See_Also: https://forum.dlang.org/post/acjltvvqhfcchpwgodqn@forum.dlang.org
 */
module nxt.dlang_traits;

private enum hasAutoRefForeach = __traits(compiles, () {
        mixin(`void f() { int[2] _ = [1, 2]; foreach (const auto ref e; _) {} }`);
    });

private enum hasRefForeach = __traits(compiles, {
        mixin(`void f() { int[2] _ = [1, 2]; foreach (const ref e; _) {} }`);
    });

unittest
{
    static assert(hasRefForeach);
    static assert(!hasAutoRefForeach);
}
