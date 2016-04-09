import datetime_ex;

/** UTC Offset.
    See also: https://en.wikipedia.org/wiki/List_of_UTC_time_offsets
    See also: http://forum.dlang.org/post/csurwdcdyfocrotojons@forum.dlang.org
*/
struct UTCOffset
{
    enum minHour = -12, maxHour = +14;
    enum minMinute = 0, maxMinute = 45;
    enum quarterValues = [00, 15, 30, 45];
    enum quarterNames = ["00", "15", "30", "45"];

    @safe pure:

    import std.traits : isSomeString;

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
            const sign = code.skipOverEither(`-`,
                                             "\u2212", "\u2011", "\u2013", // quoting
                                             `\u2212`, `\u2011`, `\u2013`,  // UTF-8
                                             `&minus;`, `&dash;`, `&ndash;`) ? -1 : +1; // HTML
            code.skipOver(" ");

            import std.algorithm.comparison : among;

            if (code.length == 4 && code[1].among!(':', '.')) // H:MM
            {
                const hour = sign*(code[0 .. 1].to!byte);
                this(cast(byte)hour,
                     code[2 .. $].to!ubyte);
            }
            else if (code.length == 5 && code[2].among!(':', '.')) // HH:MM
            {
                const hour = sign*(code[0 .. 2].to!byte);
                this(cast(byte)hour,
                     code[3 .. $].to!ubyte);
            }
            else
            {
                try
                {
                    const hour = sign*code.to!byte;
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
    in
    {
    }
    body
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

    bool opCast(U : bool)() const { return isDefined(); }
    int opCmp(in UTCOffset that) const @trusted
    {
        const a = *cast(ubyte*)&this;
        const b = *cast(ubyte*)&that;
        return a < b ? -1 : a > b ? 1 : 0;
    }

    string toString() const
    {
        if (isDefined)
        {
            import std.conv : to;
            const sign     = _hour > 0 ? `+` : _hour == 0 ? `±`: ``;
            const hour     = _hour.to!(typeof(return));
            const hourZPad = hour.length == 1 ? `0` : ``;
            const minute   = quarterNames[_quarter];
            return `UTC` ~ sign ~ hourZPad ~ hour ~ `:` ~ minute;
        }
        else
        {
            return "<Uninitialized UTCOffset>";
        }
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
    static assert(UTCOffset.sizeof == 1);

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
