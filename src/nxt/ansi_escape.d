/** ANSI escape codes and sequences.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code
 */
module nxt.ansi_escape;

@safe:

/** Abstract color.
 */
private template ColorType(uint offset)
{
    static enum Type : uint
    {
        init = 39 + offset,     ///< Default color.

        black   = 30 + offset,  ///< Black color.
        red     = 31 + offset,  ///< Red color.
        green   = 32 + offset,  ///< Green color.
        yellow  = 33 + offset,  ///< Yellow color.
        blue    = 34 + offset,  ///< Blue color.
        magenta = 35 + offset,  ///< Magenta color.
        cyan    = 36 + offset,  ///< Cyan color.
        white   = 37 + offset,  ///< White color.

        light_black   = 90 + offset, ///< Light black color.
        light_red     = 91 + offset, ///< Light red color.
        light_green   = 92 + offset, ///< Light green color.
        light_yellow  = 93 + offset, ///< Light yellow color.
        light_blue    = 94 + offset, ///< Light blue color.
        light_magenta = 95 + offset, ///< Light magenta color.
        light_cyan    = 96 + offset, ///< Light cyan color.
        light_white   = 97 + offset, ///< Light white color.
    }
}

/** Foreground color.
 */
alias FgColor = ColorType!(0).Type;

/** Background color.
 */
alias BgColor = ColorType!(10).Type;

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
    framed       = 51,             ///< Framed.
    encircled    = 52,             ///< Encircled.
    overlined    = 53,             ///< Overlined.
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

private void setFgColor(scope void delegate(scope const(char)[]) @safe sink,
                        const FgColor fgColor)
{
    final switch (fgColor)
    {
        static foreach (member; __traits(allMembers, FgColor))
        {
        case __traits(getMember, FgColor, member):
            enum _ = cast(int)__traits(getMember, FgColor, member); // avoids `std.conv.to`
            sink(_.stringof);
            return;
        }
    }
}

private void setBgColor(scope void delegate(scope const(char)[]) @safe sink,
                        const BgColor bgColor)
{
    final switch (bgColor)
    {
        static foreach (member; __traits(allMembers, BgColor))
        {
        case __traits(getMember, BgColor, member):
            enum _ = cast(int)__traits(getMember, BgColor, member); // avoids `std.conv.to`
            sink(_.stringof);
            return;
        }
    }
}

void setFormat(scope void delegate(scope const(char)[]) @safe sink,
               const FgColor fgColor,
               const BgColor bgColor,
               const SGR sgr) @safe
{
    sink("\033[");
    setSGR(sink, sgr);
    sink(";");
    setFgColor(sink, fgColor);
    sink(";");
    setBgColor(sink, bgColor);
    sink("m");
}

void resetFormat(scope void delegate(scope const(char)[]) @safe sink)
{
    sink("\033[0m");
}

void putFormattedText(scope void delegate(scope const(char)[]) @safe sink,
                      scope const(char)[] text,
                      const FgColor fgColor,
                      const BgColor bgColor,
                      const SGR sgr) @safe
{
    setFormat(sink, fgColor, bgColor, sgr); // set
    sink(text);
    resetFormat(sink);            // reset
}

class C
{
@safe:
    @property void toString(scope void delegate(scope const(char)[]) @safe sink) const @trusted
    {
        putFormattedText(sink, "XXX", FgColor.blue, BgColor.init, SGR.init);
    }
    this()
    {
    }
}

@safe unittest
{
    import std.stdio : writeln;
    import std.conv:to;
    auto c = new C();
    writeln(c.to!string);
}
