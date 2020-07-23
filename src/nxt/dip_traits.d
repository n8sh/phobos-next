/** Detect DIP-support given as compiler flags starting with `-dip`.
 */
module nxt.dip_traits;

/** Is `true` iff DIP-1000 checking is enabled via compiler flag -dip1000.
 *
 * See_Also: https://forum.dlang.org/post/qglynupcootocnnnpmhj@forum.dlang.org
 * See_Also: https://forum.dlang.org/post/pzddsrwhfvcopfaamvak@forum.dlang.org
 */
enum isDIP1000 = __traits(compiles, () @safe { int x; int* p; p = &x; });
