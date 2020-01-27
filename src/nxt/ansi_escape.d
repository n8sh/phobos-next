/** ANSI escape codes and sequences.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code
 */
module nxt.ansi_escape;

@safe:

/** SGR (Select Graphic Rendition) sets display attributes.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
 */
enum SGR : uint
{
    init         = 0,              ///< Default.
    bold         = 1,              ///< Bold or increased intensity.
    faint        = 2,              ///< Faint (decreased intensity)
    italic       = 3,              ///< Italic. Not widely supported. Sometimes treated as inverse.
    underline    = 4,              ///< Underline.
    slowBlink    = 5,              ///< Slow blink.
    rapidBlink   = 6,              ///< Rapid blink.
    reverseVideo = 7,              ///< Reversed video (swap). Swap foreground with background color.
    hide         = 8,              ///< Conceal (Hide). Not widely supported.
    crossedOut   = 9,              ///< Crossed-out. Characters legible, but marked for deletion.
    primaryDefaultFont = 10,       ///< Primary (default) font.
    fraktur      = 20,             ///< Fraktur. Rarely supported

    blackForegroundColor   = 30,  ///< Black foreground color.
    redForegroundColor     = 31,  ///< Red foreground color.
    greenForegroundColor   = 32,  ///< Green foreground color.
    yellowForegroundColor  = 33,  ///< Yellow foreground color.
    blueForegroundColor    = 34,  ///< Blue foreground color.
    magentaForegroundColor = 35,  ///< Magenta foreground color.
    cyanForegroundColor    = 36,  ///< Cyan foreground color.
    whiteForegroundColor   = 37,  ///< White foreground color.

    defaultForegroundColor = 39,  ///< Default foreground color.

    lightBlackForegroundColor   = 90, ///< Light black foreground color.
    lightRedForegroundColor     = 91, ///< Light red foreground color.
    lightGreenForegroundColor   = 92, ///< Light green foreground color.
    lightYellowForegroundColor  = 93, ///< Light yellow foreground color.
    lightBlueForegroundColor    = 94, ///< Light blue foreground color.
    lightMagentaForegroundColor = 95, ///< Light magenta foreground color.
    lightCyanForegroundColor    = 96, ///< Light cyan foreground color.
    lightWhiteForegroundColor   = 97, ///< Light white foreground color.

    defaultBackgroundColor = 49, ///< Default background color.

    framed       = 51,             ///< Framed.
    encircled    = 52,             ///< Encircled.
    overlined    = 53,             ///< Overlined.
    notFramedOrEncircled = 54,     ///< Not framed or encircled.
    notOverlined = 55,             ///< Not overlined.
    IdeogramUnderlineOrRightSideLine = 60, ///< Ideogram underline or right side line.
}

private void setSGR(scope void delegate(scope const(char)[]) @safe sink,
                    const SGR sgr)
{
    final switch (sgr)
    {
        static foreach (member; __traits(allMembers, SGR))
        {
        case __traits(getMember, SGR, member):
            enum _ = cast(int)__traits(getMember, SGR, member); // avoids `std.conv.to`
            sink(_.stringof);
            return;
        }
    }
}

void setSGRs(scope void delegate(scope const(char)[]) @safe sink,
             scope const SGR[] sgrs...) @safe
{
    sink("\033[");
    const n = sgrs.length;
    foreach (const index, const sgr; sgrs)
    {
        setSGR(sink, sgr);      // needs to be first
        if (index != n - 1)     // if not last
        {
            sink(";");          // separator
        }
    }
    sink("m");
}

void resetSGRs(scope void delegate(scope const(char)[]) @safe sink)
{
    sink("\033[0m");
}

void putWithSGRs(scope void delegate(scope const(char)[]) @safe sink,
                 scope const(char)[] text,
                 scope const SGR[] sgrs...) @safe
{
    setSGRs(sink, sgrs);        // set
    sink(text);
    resetSGRs(sink);            // reset
}

/** RGB 24-bit color.
 */
struct RGB
{
    ubyte red;                  ///< Red component.
    ubyte green;                ///< Green component.
    ubyte blue;                 ///< Blue component.
}

/** Set foreground color.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
 */
void setForegroundColorRGB(scope void delegate(scope const(char)[]) @safe sink,
                           const RGB rgb) @safe
{
    sink("\033[ 38;2;");
    import std.conv : to;
    setColorRGBComponent(sink, rgb.red);
    sink(";");
    setColorRGBComponent(sink, rgb.green);
    sink(";");
    setColorRGBComponent(sink, rgb.blue);
    sink(" m");
}

/** Set background color.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
 */
void setBackgroundColorRGB(scope void delegate(scope const(char)[]) @safe sink,
                           const RGB rgb) @safe
{
    sink("\033[ 48;2;");
    import std.conv : to;
    setColorRGBComponent(sink, rgb.red);
    sink(";");
    setColorRGBComponent(sink, rgb.green);
    sink(";");
    setColorRGBComponent(sink, rgb.blue);
    sink(" m");
}

/** Set RGB 24-bit foreground color.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
 */
static private void setColorRGBComponent(scope void delegate(scope const(char)[]) @safe sink,
                                         ubyte component) @safe
{
}

class C
{
@safe:
    @property void toString(scope void delegate(scope const(char)[]) @safe sink) const @trusted
    {
        putWithSGRs(sink, "XXX", SGR.yellowForegroundColor);
    }
    this() {}
}

@safe unittest
{
    import std.stdio : writeln;
    import std.conv:to;
    auto c = new C();
    writeln(c.to!string);
}
