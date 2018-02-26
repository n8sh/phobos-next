import datetime_ex;

/** UTC Offset.
    See also: https://en.wikipedia.org/wiki/List_of_UTC_time_offsets
    See also: http://forum.dlang.org/post/csurwdcdyfocrotojons@forum.dlang.org
*/
struct UTCOffset
{
    import std.traits : isSomeString;

    enum minHour = -12, maxHour = +14;
    enum minMinute = 0, maxMinute = 45;
    enum quarterValues = [00, 15, 30, 45];
    enum quarterNames = ["00", "15", "30", "45"];

    @safe pure:

    this(S)(S code, bool strictFormat = false)
        if (isSomeString!S)
    {
        import std.conv : to;

        import skip_ex : skipOverEither;
        import std.algorithm.searching : startsWith, skipOver;

        // TODO support and use CT-arguments in skipOverEither()
        if (strictFormat && !code.startsWith("UTC"))
        {
            this(0, 0);
            _initializedFlag = false;
        }
        else
        {
            code.skipOverEither("UTC", "GMT");

            code.skipOver(" ");
            code.skipOverEither("+", "±", `\u00B1`, "\u00B1");
            // try in order of probability
            immutable sign = code.skipOverEither(`-`,
                                             "\u2212", "\u2011", "\u2013", // quoting
                                             `\u2212`, `\u2011`, `\u2013`,  // UTF-8
                                             `&minus;`, `&dash;`, `&ndash;`) ? -1 : +1; // HTML
            code.skipOver(" ");

            import std.algorithm.comparison : among;

            if (code.length == 4 && code[1].among!(':', '.')) // H:MM
            {
                immutable hour = sign*(code[0 .. 1].to!byte);
                this(cast(byte)hour,
                     code[2 .. $].to!ubyte);
            }
            else if (code.length == 5 && code[2].among!(':', '.')) // HH:MM
            {
                immutable hour = sign*(code[0 .. 2].to!byte);
                this(cast(byte)hour,
                     code[3 .. $].to!ubyte);
            }
            else
            {
                try
                {
                    immutable hour = sign*code.to!byte;
                    this(cast(byte)hour, 0);
                }
                catch (Exception E)
                {
                    _initializedFlag = false;
                }
            }
        }

    }

    nothrow:

    this(byte hour, ubyte minute)
    {
        if (hour >= minHour &&
            hour <= maxHour &&
            minute >= minMinute &&
            minute <= maxMinute)
        {
            _hour = hour;
            _initializedFlag = true;
            switch (minute)
            {
            case 0: _quarter = 0; break;
            case 15: _quarter = 1; break;
            case 30: _quarter = 2; break;
            case 45: _quarter = 3; break;
            default: _initializedFlag = false; break;
            }
        }
        else
        {
            _hour = 0;
            _quarter = 0;
            _initializedFlag = false;
        }
    }

    string toString() const
    {
        if (isDefined)
        {
            import std.conv : to;
            immutable sign     = _hour > 0 ? `+` : _hour == 0 ? `±`: ``;
            immutable hour     = _hour.to!(typeof(return));
            immutable hourZPad = hour.length == 1 ? `0` : ``;
            immutable minute   = quarterNames[_quarter];
            return `UTC` ~ sign ~ hourZPad ~ hour ~ `:` ~ minute;
        }
        else
        {
            return "<Uninitialized UTCOffset>";
        }
    }

    pragma(inline):

    /// Cast to `bool`, meaning 'true' if defined, `false` otherwise.
    bool opCast(U : bool)() const
    {
        return isDefined();
    }

    int opCmp(in typeof(this) that) const @trusted
    {
        immutable a = *cast(ubyte*)&this;
        immutable b = *cast(ubyte*)&that;
        return a < b ? -1 : a > b ? 1 : 0;
    }

    @property:

    auto hour()      const { return _hour; }
    auto minute()    const { return _quarter * 15; }
    bool isDefined() const { return _initializedFlag; }

private:
    import std.bitmanip : bitfields;
    mixin(bitfields!(byte, "_hour", 5, // Hours: [-12 up to +14]
                     ubyte, "_quarter", 2, // Minutes in Groups of 15: 0, 15, 30, 45
                     bool, "_initializedFlag", 1));
}

@safe pure // nothrow
unittest
{
    static assert(UTCOffset.sizeof == 1); // assert packet storage

    assert(UTCOffset(-12, 0));

    assert(UTCOffset(-12, 0) !=
           UTCOffset(  0, 0));

    assert(UTCOffset(-12, 0) ==
           UTCOffset(-12, 0));

    assert(UTCOffset(-12, 0) <
           UTCOffset(-11, 0));

    assert(UTCOffset(-11, 0) <=
           UTCOffset(-11, 0));

    assert(UTCOffset(-11, 0) <=
           UTCOffset(-11, 15));

    assert(UTCOffset(-12, 0) <
           UTCOffset(+14, 15));

    assert(UTCOffset(+14, 15) <=
           UTCOffset(+14, 15));

    assert(UTCOffset(-12, 0).hour == -12);
    assert(UTCOffset(+14, 0).hour == +14);

    assert(UTCOffset("").toString == "<Uninitialized UTCOffset>");
    assert(UTCOffset(UTCOffset.minHour - 1, 0).toString == "<Uninitialized UTCOffset>");
    assert(UTCOffset(UTCOffset.maxHour + 1, 0).toString == "<Uninitialized UTCOffset>");
    assert(UTCOffset(UTCOffset.minHour, 1).toString == "<Uninitialized UTCOffset>");
    assert(UTCOffset(UTCOffset.minHour, 46).toString == "<Uninitialized UTCOffset>");

    assert(UTCOffset(-12, 0).toString == "UTC-12:00");
    assert(UTCOffset(+00, 0).toString == "UTC±00:00");
    assert(UTCOffset(+07, 0).toString == "UTC+07:00");
    assert(UTCOffset(+14, 0).toString == "UTC+14:00");

    import std.conv : to;
    assert(UTCOffset(+14, 0).to!string == "UTC+14:00");

    assert(UTCOffset("-1"));
    assert(UTCOffset(-12, 0) == UTCOffset("-12"));
    assert(UTCOffset(-12, 0) == UTCOffset("\u221212"));
    assert(UTCOffset(+14, 0) == UTCOffset("+14"));

    assert(UTCOffset(+14, 0) == UTCOffset("UTC+14"));

    assert(UTCOffset(+03, 30) == UTCOffset("+3:30"));
    assert(UTCOffset(+03, 30) == UTCOffset("+03:30"));
    assert(UTCOffset(+14, 00) == UTCOffset("UTC+14:00"));

    assert(UTCOffset(+14, 00) == "UTC+14:00".to!UTCOffset);

    assert(!UTCOffset("+14:00", true)); // strict faiure
    assert(UTCOffset("UTC+14:00", true)); // strict pass
}

/** Year and Month.

    If month is specified we probably aren't interested in years before 0 so
    store only years 0 .. 2^12-1 (4095). This makes this struct fit in 2 bytes.
 */
struct YearMonth
{
    import std.traits : isSomeString;
    import std.datetime : Month;

    import std.bitmanip : bitfields;
    mixin(bitfields!(ushort, "year", 12,
                     Month, "month", 4));

    pragma(inline) this(int year, Month month)
        @safe pure nothrow @nogc
    {
        assert(0 <= year && year <= 2^^12 - 1); // assert within range
        this.year = cast(ushort)year;
        this.month = month;
    }

    /// No explicit destruction needed.
    ~this() @safe pure nothrow @nogc {} // needed for @nogc use

    this(S)(S s)
        if (isSomeString!S)
    {
        import std.algorithm.searching : findSplit;
        auto parts = s.findSplit(` `); // TODO s.findSplitAtElement(' ')
        if (parts &&
            parts[0].length >= 3) // at least three letters in month
        {
            import std.conv : to;

            // decode month
            import std.traits : Unqual;
            import casing : toLowerASCII;
            Unqual!(typeof(S.init[0])[3]) tmp = parts[0][0 .. 3]; // TODO functionize to parts[0].staticSubArray!(0, 3)
            import std.ascii : toLower;
            tmp[0] = tmp[0].toLower;
            month = tmp.to!Month;

            // decode year
            year = parts[2].to!(typeof(year));

            return;
        }

        import std.conv;
        throw new std.conv.ConvException("Couldn't decode year and month from string");
    }

    @safe pure:

    @property string toString() const
    {
        import std.conv : to;
        return year.to!string ~ `-` ~ (cast(ubyte)month).to!string; // TODO avoid GC allocation
    }

pragma(inline):

    int opCmp(in typeof(this) that) const nothrow @nogc
    {
        if (this.year < that.year)
        {
            return -1;
        }
        else if (this.year > that.year)
        {
            return +1;
        }
        else
        {
            if (this.month < that.month)
            {
                return -1;
            }
            else if (this.month > that.month)
            {
                return +1;
            }
            else
            {
                return 0;
            }
        }
    }
}

@safe pure /*TODO @nogc*/ unittest
{
    import std.datetime : Month;
    Month month;

    static assert(YearMonth.sizeof == 2); // assert packed storage

    const a = YearMonth(`April 2016`);

    assert(a != YearMonth.init);

    assert(a == YearMonth(2016, Month.apr));
    assert(a != YearMonth(2016, Month.may));
    assert(a != YearMonth(2015, Month.apr));

    assert(a.year == 2016);
    assert(a.month == Month.apr);

    assert(YearMonth(`April 1900`) == YearMonth(1900, Month.apr));
    assert(YearMonth(`april 1900`) == YearMonth(1900, Month.apr));
    assert(YearMonth(`apr 1900`) == YearMonth(1900, Month.apr));
    assert(YearMonth(`Apr 1900`) == YearMonth(1900, Month.apr));

    assert(YearMonth(`Apr 1900`) != YearMonth(1901, Month.apr));
    assert(YearMonth(`Apr 1900`) < YearMonth(1901, Month.apr));
    assert(YearMonth(`Apr 1901`) > YearMonth(1900, Month.apr));

    assert(YearMonth(`Apr 1900`) < YearMonth(1901, Month.may));

    assert(YearMonth(`Apr 1900`) < YearMonth(1901, Month.may));
    assert(YearMonth(`May 1900`) < YearMonth(1901, Month.apr));
}
