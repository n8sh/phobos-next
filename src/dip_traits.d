module dip_traits;

/** Is `true` iff DIP-1000 checking is enabled via compiler flag -dip1000.
 *
 * See_Also: https://forum.dlang.org/post/qglynupcootocnnnpmhj@forum.dlang.org
 */
enum isDIP1000 = __traits(compiles, () @safe { int x; int* p; p = &x; });
