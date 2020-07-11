/** Colors.
 *
 * See_Also: https://github.com/TurkeyMan/color
 */
module nxt.color;

@safe pure nothrow @nogc:

/** RGB 24-bit color, where each color component has 8-bit precision.
 *
 * See_Also: Implements the $(LINK2 https://en.wikipedia.org/wiki/RGB_color_space, RGB) _color type.
 */
struct ColorRGB8
{
    ubyte red;                  ///< Red component.
    ubyte green;                ///< Green component.
    ubyte blue;                 ///< Blue component.
}

alias Color = ColorRGB8;

static immutable white = Color(0xff, 0xff, 0xff); ///< White.
static immutable black = Color(0x00, 0x00, 0x00); ///< Black.

/** BGR 24-bit color, where each color component has 8-bit precision.
 */
struct ColorBGR8
{
    ubyte blue;                 ///< Blue component.
    ubyte green;                ///< Green component.
    ubyte red;                  ///< Red component.
}
