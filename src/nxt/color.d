/** Colors.
 *
 * See_Also: https://github.com/TurkeyMan/color
 */
module nxt.color;

@safe pure nothrow @nogc:

/** RGB 24-bit color.
 *
 * See_Also: https://github.com/TurkeyMan/color
 */
struct ColorRGB
{
    ubyte red;                  ///< Red component.
    ubyte green;                ///< Green component.
    ubyte blue;                 ///< Blue component.
}
