/** ANSI escape codes and sequences.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code
 */
module nxt.ansi_escape;

@safe:

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

/** Foreground.
 */
alias FgColor = ColorType!(0).Type;

/** Background.
 */
alias BgColor = ColorType!(10).Type;

/** SGR (Select Graphic Rendition) sets display attributes.
 *
 * See_Also: https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
 */
enum Mode : uint
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
    corssedOut   = 9,              ///< Crossed-out. Characters legible, but marked for deletion.
}

void setFormat(scope void delegate(scope const(char)[]) @safe sink,
               const FgColor fgColor = FgColor.init,
               const BgColor bgColor = BgColor.init,
               const Mode mode = Mode.init) @safe
{
    sink("\033[");

    // mode
    final switch (mode)
    {
        static foreach (member; __traits(allMembers, Mode))
        {
        case __traits(getMember, Mode, member):
            enum _ = cast(int)__traits(getMember, Mode, member); // avoids `std.conv.to`
            sink(_.stringof);
        }
    }
    sink(";");

    // fgColor
    final switch (fgColor)
    {
        static foreach (member; __traits(allMembers, FgColor))
        {
        case __traits(getMember, FgColor, member):
            enum _ = cast(int)__traits(getMember, FgColor, member); // avoids `std.conv.to`
            sink(_.stringof);
        }
    }
    sink(";");

    // bgColor
    final switch (bgColor)
    {
        static foreach (member; __traits(allMembers, BgColor))
        {
        case __traits(getMember, BgColor, member):
            enum _ = cast(int)__traits(getMember, BgColor, member); // avoids `std.conv.to`
            sink(_.stringof);
        }
    }
    sink(";");

    sink("m");
}

void resetFormat(scope void delegate(scope const(char)[]) @safe sink) @safe
{
    sink("\033[0m");
}

void putFormattedText(scope void delegate(scope const(char)[]) @safe sink,
                      scope return inout(char)[] text,
                      const FgColor fgColor = FgColor.init,
                      const BgColor bgColor = BgColor.init,
                      const Mode mode = Mode.init) @safe
{
    setFormat(sink, fgColor, bgColor, mode); // set
    sink(text);
    resetFormat(sink);            // reset
}
