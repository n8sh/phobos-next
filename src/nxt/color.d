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

static immutable black   = Color(0x00, 0x00, 0x00); ///< Black.
static immutable white   = Color(0xff, 0xff, 0xff); ///< White.
static immutable red     = Color(0xff, 0x00, 0x00); ///< Red.
static immutable green   = Color(0x00, 0xff, 0x00); ///< Green.
static immutable blue    = Color(0x00, 0x00, 0xff); ///< Blue.
static immutable cyan    = Color(0x00, 0xff, 0xff); ///< Cyan.
static immutable magenta = Color(0xff, 0x00, 0xff); ///< Magenta.
static immutable yellow  = Color(0xff, 0xff, 0x00); ///< Yellow.

/** BGR 24-bit color, where each color component has 8-bit precision.
 */
struct ColorBGR8
{
    ubyte blue;                 ///< Blue component.
    ubyte green;                ///< Green component.
    ubyte red;                  ///< Red component.
}
