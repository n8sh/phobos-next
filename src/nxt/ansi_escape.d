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

/** Foreground. */
alias Fg = ColorType!(0).Type;

/** Background. */
alias Bg = ColorType!(10).Type;

/** ANSI Color Mode. */
static enum Mode : uint
{
    init      = 0,              ///< Default.
    bold      = 1,              ///< Bold.
    underline = 4,              ///< Underline.
    blink     = 5,              ///< Blink.
    swap      = 7,              ///< Swap.
    hide      = 8,              ///< Hide.
}

void setFormat(scope void delegate(scope const(char)[]) @safe sink,
               const Fg fg = Fg.init,
               const Bg bg = Bg.init,
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

    // fg
    final switch (fg)
    {
        static foreach (member; __traits(allMembers, Fg))
        {
        case __traits(getMember, Fg, member):
            enum _ = cast(int)__traits(getMember, Fg, member); // avoids `std.conv.to`
            sink(_.stringof);
        }
    }
    sink(";");

    // bg
    final switch (bg)
    {
        static foreach (member; __traits(allMembers, Bg))
        {
        case __traits(getMember, Bg, member):
            enum _ = cast(int)__traits(getMember, Bg, member); // avoids `std.conv.to`
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
                      const Fg fg = Fg.init,
                      const Bg bg = Bg.init,
                      const Mode mode = Mode.init) @safe
{
    setFormat(sink, fg, bg, mode); // set
    sink(text);
    resetFormat(sink);            // reset
}
