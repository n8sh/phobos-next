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
    ubyte redC;                  ///< Red component.
    ubyte greenC;                ///< Green component.
    ubyte blueC;                 ///< Blue component.
    static immutable black   = Color(0x00, 0x00, 0x00); ///< Black.
    static immutable white   = Color(0xff, 0xff, 0xff); ///< White.
    static immutable red     = Color(0xff, 0x00, 0x00); ///< Red.
    static immutable green   = Color(0x00, 0xff, 0x00); ///< Green.
    static immutable blue    = Color(0x00, 0x00, 0xff); ///< Blue.
    static immutable cyan    = Color(0x00, 0xff, 0xff); ///< Cyan.
    static immutable magenta = Color(0xff, 0x00, 0xff); ///< Magenta.
    static immutable yellow  = Color(0xff, 0xff, 0x00); ///< Yellow.
}

alias Color = ColorRGB8;

/** BGR 24-bit color, where each color component has 8-bit precision.
 */
struct ColorBGR8
{
    ubyte blueC;                ///< Blue component.
    ubyte greenC;               ///< Green component.
    ubyte redC;                 ///< Red component.
}
