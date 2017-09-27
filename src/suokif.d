/** SUO-KIF File Format.

    See also: https://en.wikipedia.org/wiki/Knowledge_Interchange_Format
    See also: http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
    See also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
*/
module suokif;

version = benchmark;

/** SUO-KIF (Lisp) Token Type. */
enum TOK
{
    unknown,

    leftParen,
    rightParen,

    symbol,

    stringLiteral,

    lispComma,
    lispBackQuote,
    lispQuote,

    variable,
    variableList,               // one or more variables (parameters)
    functionName,

    number,

    comment,
    whitespace,
}

/** SUO-KIF Token. */
struct Token
{
    @safe pure nothrow @nogc:
    TOK tok;
    string src;                 // optional source slice
}

/** SUO_KIF Expression. */
struct Expr
{
    Token token;
    Expr[] subs;
}

import arrayn : ArrayN, Checking;
import file_ex : rawReadNullTerminated;

alias Exprs = ArrayN!(Expr, 128, Checking.viaScope);

/** Returns: true if `s` is null-terminated (ending with `'\0'`).

    Prior to parsing used to verify input to parsers that make use of
    sentinel-based search.

    See also: https://en.wikipedia.org/wiki/Sentinel_value
 */
pragma(inline, true)
bool isNullTerminated(const(char)[] s)
    @safe pure nothrow @nogc
{
    return s.length >= 1 && s[$ - 1] == '\0';
}

/** SUO-KIF parse from `input` into lazy range over top-level expressions (`Expr`).
 */
struct SUOKIFParser
{
    import std.algorithm : among;

    private alias Src = string;

    @safe pure:

    /** Parse SUO-KIF from `input` into returned array of expressions (`Expr`).
     */
    this(Src input,
         bool includeComments = false,
         bool includeWhitespace = false,
         bool disallowEmptyLists = false)
    {
        _input = input;

        import std.algorithm : startsWith;
        immutable magic = x"EFBBBF";
        if (_input[_offset .. $].startsWith(magic))
        {
            _offset += magic.length;
        }

        import std.exception : enforce;
        enforce(_input.isNullTerminated); // safest to do this check in non-debug mode aswell

        _includeComments = includeComments;
        _includeWhitespace = includeWhitespace;
        _disallowEmptyLists = disallowEmptyLists;

        nextFront();
    }

    pragma(inline, true)
    @property bool empty() const nothrow @nogc
    {
        return _endOfFile;
    }

    pragma(inline, true)
    ref Expr front()            // TODO return scope
    {
        assert(!empty);
        return exprs.back;
    }

    pragma(inline, true)
    void popFront()
    {
        assert(!empty);
        exprs.popBack();
        nextFront();
    }

    import std.meta : AliasSeq;

    // from std.ascii.isWhite
    alias endOfLineChars = AliasSeq!('\n', // (0x0a)
                                     '\r', // (0x0c)
        );
    alias whiteChars = AliasSeq!(' ', // 0x20
                                 '\t', // (0x09)
                                 '\n', // (0x0a)
                                 '\v', // (0x0b)
                                 '\r', // (0x0c)
                                 '\f' // (0x0d)
        );
    alias digitChars = AliasSeq!('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

private:

    /// Get next `char` in input.
    pragma(inline, true)
    char peekNext() const nothrow @nogc
    {
        return _input[_offset];    // TODO .ptr
    }

    /// Get next `char` in input.
    pragma(inline, true)
    char peekNextNth(size_t n) const nothrow @nogc
    {
        return _input[_offset + n]; // TODO .ptr
    }

    /// Get next n `chars` in input.
    pragma(inline, true)
    Src peekNextsN(size_t n) const nothrow @nogc
    {
        return _input[_offset .. _offset + n]; // TODO .ptr
    }

    /// Drop next byte in input.
    pragma(inline, true)
    void dropFront() nothrow @nogc
    {
        _offset += 1;
    }

    /// Drop next `n` bytes in input.
    pragma(inline, true)
    void dropFrontN(size_t n) nothrow @nogc
    {
        _offset += n;
    }

    /// Skip over `n` bytes in `src`.
    pragma(inline)
    Src skipOverN(size_t n) nothrow @nogc
    {
        const part = _input[_offset .. _offset + n]; // TODO .ptr
        dropFrontN(n);
        return part;
    }

    /// Skip line comment.
    pragma(inline)
    void skipLineComment() nothrow @nogc
    {
        while (!peekNext().among('\0', endOfLineChars))
        {
            _offset += 1;
        }
    }

    /// Get symbol.
    pragma(inline)
    Src getSymbol() nothrow @nogc
    {
        size_t i = 0;
        while ((!peekNextNth(i).among!('\0', '(', ')',
                                       whiteChars))) // NOTE this is faster than !src[i].isWhite
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get numeric literal (number) in integer or decimal form.
    pragma(inline)
    Src getNumber() nothrow @nogc
    {
        size_t i = 0;
        while (peekNextNth(i).among!('+', '-', '.',
                                     digitChars)) // NOTE this is faster than src[i].isDigit
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get whitespace.
    pragma(inline)
    Src getWhitespace() nothrow @nogc
    {
        size_t i = 0;
        while (peekNextNth(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get string literal at `src`.
    pragma(inline)
    Src getStringLiteral() nothrow @nogc
    {
        dropFront();
        size_t i = 0;
        while (!peekNextNth(i).among('\0', '"')) // TODO handle backslash + double-quote
        {
            ++i;
        }
        const literal = peekNextsN(i);
        dropFrontN(i);
        if (peekNext() == '"') { dropFront(); } // pop ending double quote
        return literal;
    }

    void nextFront()
    {
        import std.range : empty, front, popFront, popFrontN;
        import std.uni : isWhite, isAlpha;
        import std.ascii : isDigit;

        while (true)
        {
            switch (_input[_offset]) // TODO .ptr
            {
            case ';':
                skipLineComment();  // TODO store comment in Token
                if (_includeComments)
                {
                    assert(false, "change skipLineComment");
                    // exprs.put(Expr(Token(TOK.comment, src[0 .. 1])));
                }
                break;
            case '(':
                exprs.put(Expr(Token(TOK.leftParen, peekNextsN(1))));
                dropFront();
                ++_depth;
                break;
            case ')':
                // NOTE: this is not needed: exprs.put(Expr(Token(TOK.rightParen, src[0 .. 1])));
                dropFront();
                --_depth;
                // NOTE: this is not needed: exprs.popBack();   // pop right paren

                assert(!exprs.empty);

                // TODO retroIndexOf
                size_t count; // number of elements between parens
                while (exprs[$ - 1 - count].token.tok != TOK.leftParen)
                {
                    ++count;
                }
                if (_disallowEmptyLists)
                {
                    assert(count != 0);
                }

                Expr newExpr = Expr(exprs[$ - count].token,
                                    count ? exprs[$ - count + 1 .. $].dup : []);
                exprs.popBackN(1 + count); // forget tokens including leftParen
                import std.algorithm : move;
                exprs.put(newExpr.move);

                if (_depth == 0) // top-level expression done
                {
                    assert(exprs.length >= 1); // we should have at least one `Expr`
                    return;
                }

                break;
            case '"':
                const stringLiteral = getStringLiteral(); // TODO tokenize
                exprs.put(Expr(Token(TOK.stringLiteral, stringLiteral)));
                break;
            case ',':
                dropFront();
                exprs.put(Expr(Token(TOK.lispComma)));
                break;
            case '`':
                dropFront();
                exprs.put(Expr(Token(TOK.lispBackQuote)));
                break;
            case '\'':
                dropFront();
                exprs.put(Expr(Token(TOK.lispQuote)));
                break;
            case '?':
                dropFront();
                const variableSymbol = getSymbol();
                exprs.put(Expr(Token(TOK.variable, variableSymbol)));
                break;
            case '@':
                dropFront();
                const variableListSymbol = getSymbol();
                exprs.put(Expr(Token(TOK.variableList, variableListSymbol)));
                break;
                // std.ascii.isDigit:
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            case '+':
            case '-':
            case '.':
                const number = getNumber();
                exprs.put(Expr(Token(TOK.number, number)));
                break;
                // from std.ascii.isWhite
            case ' ':
            case '\t':
            case '\n':
            case '\v':
            case '\r':
            case '\f':
                assert(peekNext.isWhite);
                getWhitespace();
                if (_includeWhitespace)
                {
                    exprs.put(Expr(Token(TOK.whitespace, null)));
                }
                break;
            case '\0':
                assert(_depth == 0, "Unbalanced parenthesis at end of file");
                _endOfFile = true;
                return;
            default:
                // other
                if (true// src.front.isAlpha
                    )
                {
                    const symbol = getSymbol(); // TODO tokenize
                    import std.algorithm : endsWith;
                    if (symbol.endsWith(`Fn`))
                    {
                        exprs.put(Expr(Token(TOK.functionName, symbol)));
                    }
                    else
                    {
                        exprs.put(Expr(Token(TOK.symbol, symbol)));
                    }
                }
                else
                {
                    import std.conv : to;
                    assert(false,
                           `Cannot handle character '` ~ peekNext.to!string ~
                           `' at charater offset:` ~ _offset.to!string);
                }
                break;
            }
        }
    }

private:
    size_t _offset;             // current offset in `_input`
    const Src _input;           // input

    Exprs exprs;                // current

    size_t _depth;              // parenthesis depth
    bool _endOfFile;            // signals null terminator found
    bool _includeComments = false;
    bool _includeWhitespace = false;
    bool _disallowEmptyLists = false;
}

@safe pure unittest
{
    const text = ";;a comment\n(instance AttrFn BinaryFunction);;another comment\0";
    auto exprs = SUOKIFParser(text);
    assert(!exprs.empty);

    assert(exprs.front.token.tok == TOK.symbol);
    assert(exprs.front.token.src == `instance`);

    assert(exprs.front.subs[0].token.tok == TOK.functionName);
    assert(exprs.front.subs[0].token.src == "AttrFn");

    assert(exprs.front.subs[1].token.tok == TOK.symbol);
    assert(exprs.front.subs[1].token.src == "BinaryFunction");

    exprs.popFront();
    assert(exprs.empty);
}

version(none)
unittest
{
    import std.path : expandTilde;
    import std.file : readText;
    const text = `~/elisp/mine/relangs.el`.expandTilde.readText;
    const ctext = text ~ '\0'; // null at the end to enable sentinel-based search in parser
    assert(ctext[$ - 1] == '\0');
    foreach (const ref expr; SUOKIFParser(ctext, false, false, true))
    {
    }
}

/** Read all SUO-KIF files (.kif) located under `rootDirPath`.
 */
version(benchmark)
unittest
{
    import std.stdio : write, writeln;
    import std.path : expandTilde, pathSplitter;
    import std.file: dirEntries, SpanMode;
    import std.conv : to;
    import std.datetime : StopWatch, AutoStart, Duration;
    import std.algorithm : endsWith, canFind;
    import std.utf;

    string rootDirPath = `~/Work/sumo`;

    auto totalSw = StopWatch(AutoStart.yes);

    auto entries = dirEntries(rootDirPath.expandTilde, SpanMode.breadth, false); // false: skip symlinks
    foreach (dent; entries)
    {
        const filePath = dent.name;
        try
        {
            if (filePath.endsWith(`.kif`) &&
                !filePath.pathSplitter.canFind(`.git`)) // invalid UTF-8 encodings
            {
                write(`Reading SUO-KIF `, filePath, ` ... `);
                import std.file : readText;
                auto sw = StopWatch(AutoStart.yes);
                foreach (const ref topExpr; SUOKIFParser(cast(string)filePath.rawReadNullTerminated()))
                {
                    // TOOD use topExpr
                }
                sw.stop();
                writeln(`took `, sw.peek.to!Duration);
            }
        }
        catch (std.utf.UTFException e)
        {
            import std.file : read;
            writeln("Failed because of invalid UTF-8 encoding starting with ", filePath.read(16));
        }
    }

    totalSw.stop();
    writeln(`Reading all files took `, totalSw.peek.to!Duration);
}
