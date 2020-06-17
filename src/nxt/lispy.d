/** Lexer and parser of Lisp-like languages, including SUO-KIF and Emacs-Lisp.
 *
 * See_Also: https://www.csee.umbc.edu/csee/research/kif/
 * See_Also: https://en.wikipedia.org/wiki/Knowledge_Interchange_Format
 * See_Also: http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
 * See_Also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
 *
 * TODO: Try infinite loops with break or goto instead of for loops.
 *
 * TODO: Should we add `LispFile.bySExpr` to allow use of `offsetTo` inside
 * `LispFileParser lfp; foreach (lfp.bySExpr)` now that copy-ctor is disabled
 * for `LispParser`?
 */
module nxt.lispy;

/** Lisp-like token type. */
enum TOK
{
    unknown,                    ///< Unknown.

    leftParen,                  ///< Left parenthesis.
    rightParen,                 ///< Right parenthesis.

    symbol,                     ///< Symbol.

    stringLiteral,              ///< String literal.

    comma,                      ///< Lisp comma expression, `,`.
    backquote,                  ///< Lisp backquote expression, `\``.
    singlequote,                ///< Lisp singlequote expression, `'`.

    variable,
    variableList, ///< one or more variables (parameters) starting with an at-sign, for instance `@ROW`
    functionName,

    number,                     ///< number as integer or floating point literal.

    comment,                    ///< Comment (to end of line).
    whitespace,                 ///< Whitespace.

    emptyList,                  ///< Empty list.
}

/** Lisp-like token. */
struct Token
{
@safe:
    this(TOK tok, const(char)[] src = null) pure nothrow @nogc
    {
        this.tok = tok;
        this.src = src;
    }

    @property final void toString(scope void delegate(scope const(char)[]) @safe sink) const @safe
    {
        switch (tok)
        {
        case TOK.symbol:
            sink(src);
            break;
        case TOK.comma:
            sink(`,`);
            break;
        case TOK.backquote:
            sink("`");
            break;
        case TOK.singlequote:
            sink("'");
            break;
        case TOK.stringLiteral:
            sink(`"`);
            sink(src);
            sink(`"`);
            break;
        case TOK.emptyList:
            sink(`()`);
            break;
        default:
            // import std.conv : to;
            // sink(tok.to!string);
            if (src)
            {
                // sink(`:`);
                sink(src);
            }
            break;
        }
    }

    TOK tok;
    const(char)[] src;          // optional source slice
}

/** Lisp-like S-expression. */
struct SExpr
{
@safe:
    @property final void toString(scope void delegate(scope const(char)[]) @safe sink) const @safe
    {
        if (subs) { sink(`(`); }

        token.toString(sink);

        TOK lastTok = TOK.unknown;
        foreach (const ref sub; subs)
        {
            import std.algorithm.comparison : among;
            if (!lastTok.among!(TOK.comma,
                                TOK.backquote,
                                TOK.singlequote))
            {
                sink(` `);
            }
            sub.toString(sink);
            lastTok = sub.token.tok;
        }

        if (subs) { sink(`)`); }
    }

    Token token;
    SExpr[] subs;
}

/** Returns: true if `s` is null-terminated (ending with `'\0'`).
 *
 * Prior to parsing used to verify input to parsers that make use of
 * sentinel-based search.
 *
 * See_Also: https://en.wikipedia.org/wiki/Sentinel_value
 */
bool isNullTerminated(scope const(char)[] s) @safe pure nothrow @nogc
{
    pragma(inline, true);
    return s.length >= 1 && s[$ - 1] == '\0';
}

/** Parse from `input` into lazy range over top-level expressions (`SExpr`).
 *
 * See_Also: https://forum.dlang.org/post/okqdldjnoyrtuizevqeo@forum.dlang.org
 */
struct LispParser               // TODO convert to `class`
{
    import std.algorithm.comparison : among;

    alias Input = const(char)[];

@safe pure:

    /** Parse `input` into returned array of expressions (`SExpr`).
     */
    this(Input input,
         bool includeComments = false,
         bool includeWhitespace = false,
         bool disallowEmptyLists = false,
         size_t subExprCountsGuess = 0) @trusted
    {
        _input = input;

        // if (subExprCountsGuess) // if guess use region
        // {
        //     _subExprsStore = new SExpr[subExprCountsGuess]; // region store
        // }

        import std.exception : enforce;
        enforce(_input.isNullTerminated, "Input isn't null-terminated"); // input cannot be trusted

        _includeComments = includeComments;
        _includeWhitespace = includeWhitespace;
        _disallowEmptyLists = disallowEmptyLists;

        nextFront();
    }

    @disable this(this);

    @property bool empty() const nothrow scope @nogc
    {
        pragma(inline, true);
        return _endOfFile;
    }

    ref const(SExpr) front() const scope return
    {
        pragma(inline, true);
        assert(!empty);
        return _topExprs.back;
    }

    void popFront()
    {
        pragma(inline, true);
        assert(!empty);
        _topExprs.popBack();
        nextFront();
    }

    @property size_t subExprsCount() @safe pure nothrow @nogc
    {
        return _subExprsCount;
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
    char peekNext() const scope nothrow @nogc
    {
        pragma(inline, true);
        return _input[_offset];    // TODO .ptr
    }

    /// Get next `char` in input.
    char peekNextNth(size_t n) const nothrow @nogc
    {
        pragma(inline, true);
        return _input[_offset + n]; // TODO .ptr
    }

    /// Get next n `chars` in input.
    Input peekNextsN(size_t n) const return nothrow @nogc
    {
        pragma(inline, true);
        return _input[_offset .. _offset + n]; // TODO .ptr
    }

    /// Drop next byte in input.
    void dropFront() nothrow @nogc
    {
        pragma(inline, true);
        _offset += 1;
    }

    /// Drop next `n` bytes in input.
    void dropFrontN(size_t n) nothrow @nogc
    {
        pragma(inline, true);
        _offset += n;
    }

    /// Skip over `n` bytes in input.
    Input skipOverN(size_t n) return nothrow @nogc
    {
        pragma(inline);
        const part = _input[_offset .. _offset + n]; // TODO .ptr
        dropFrontN(n);
        return part;
    }

    /// Skip line comment.
    void skipLineComment() scope nothrow @nogc
    {
        while (!peekNext().among!('\0', endOfLineChars))
        {
            _offset += 1;
        }
    }

    /// Get symbol.
    Input getSymbol() return nothrow @nogc
    {
        size_t i = 0;
        while ((!peekNextNth(i).among!('\0', '(', ')', '"', whiteChars))) // NOTE this is faster than !src[i].isWhite
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get numeric literal (number) in integer or decimal form.
    Input getNumberOrSymbol(out bool gotSymbol) return nothrow @nogc
    {
        size_t i = 0;
        while ((peekNextNth(i).among!('+', '-', '.', digitChars))) // NOTE this is faster than !src[i].isWhite
        {
            ++i;
        }
        import std.ascii : isAlpha;
        if (peekNextNth(i).isAlpha) // if followed by letter
        {
            size_t alphaCount = 0;
            while ((!peekNextNth(i).among!('\0', '(', ')', '"', whiteChars))) // NOTE this is faster than !src[i].isWhite
            {
                alphaCount += peekNextNth(i).isAlpha;
                ++i;
            }
            gotSymbol = alphaCount >= 2; // at least two letters, excluding floating point such as 1.0e+10
        }

        return skipOverN(i);
    }

    /// Get whitespace.
    Input getWhitespace() return nothrow @nogc
    {
        size_t i = 0;
        while (peekNextNth(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get string literal in input.
    Input getStringLiteral() return nothrow @nogc
    {
        dropFront();
        size_t i = 0;
        while (!peekNextNth(i).among!('\0', '"'))
        {
            if (peekNextNth(i) == '\\' &&
                peekNextNth(i + 1) == '"')
            {
                i += 2;         // skip \n
                continue;
            }
            ++i;
        }
        const literal = peekNextsN(i);
        dropFrontN(i);
        if (peekNext() == '"') { dropFront(); } // pop ending double singlequote
        return literal;
    }

    SExpr[] dupTopExprs(SExpr[] exprs) @safe pure nothrow
    {
        pragma(inline, true);
        _subExprsCount += exprs.length; // log it for future optimizations
        return exprs.dup; // TODO use region allocator stored locally in `LispParser`
    }

    void nextFront()
    {
        import std.range.primitives : empty, front, popFront, popFrontN;
        import std.uni : isWhite, isAlpha;
        import std.ascii : isDigit;

        while (true)
        {
            switch (_input[_offset]) // TODO .ptr
            {
            case ';':
                if (_includeComments)
                {
                    assert(0, "TODO don't use skipLineComment");
                    // _topExprs.insertBack(SExpr(Token(TOK.comment, src[0 .. 1])));
                }
                else
                {
                    skipLineComment();
                }
                break;
            case '(':
                _topExprs.insertBack(SExpr(Token(TOK.leftParen, peekNextsN(1))));
                dropFront();
                ++_depth;
                break;
            case ')':
                // NOTE: this is not needed: _topExprs.insertBack(SExpr(Token(TOK.rightParen, src[0 .. 1])));
                dropFront();
                --_depth;
                // NOTE: this is not needed: _topExprs.popBack();   // pop right paren

                assert(!_topExprs.empty);

                // TODO retroIndexOf
                size_t count; // number of elements between parens
                while (_topExprs[$ - 1 - count].token.tok != TOK.leftParen)
                {
                    ++count;
                }
                if (_disallowEmptyLists)
                {
                    assert(count != 0);
                }

                import core.lifetime : move;
                SExpr newExpr = ((count == 0) ?
                                 SExpr(Token(TOK.emptyList)) :
                                 SExpr(_topExprs[$ - count].token,
                                       dupTopExprs(_topExprs[$ - count + 1 .. $])));
                _topExprs.popBackN(1 + count); // forget tokens including leftParen
                _topExprs.insertBack(newExpr.move);

                if (_depth == 0) // top-level expression done
                {
                    assert(_topExprs.length >= 1); // we should have at least one `SExpr`
                    return;
                }

                break;
            case '"':
                const stringLiteral = getStringLiteral(); // TODO tokenize
                _topExprs.insertBack(SExpr(Token(TOK.stringLiteral, stringLiteral)));
                break;
            case ',':
                dropFront();
                _topExprs.insertBack(SExpr(Token(TOK.comma)));
                break;
            case '`':
                dropFront();
                _topExprs.insertBack(SExpr(Token(TOK.backquote)));
                break;
            case '\'':
                dropFront();
                _topExprs.insertBack(SExpr(Token(TOK.singlequote)));
                break;
            case '?':
                dropFront();
                const variableSymbol = getSymbol();
                _topExprs.insertBack(SExpr(Token(TOK.variable, variableSymbol)));
                break;
            case '@':
                dropFront();
                const variableListSymbol = getSymbol();
                _topExprs.insertBack(SExpr(Token(TOK.variableList, variableListSymbol)));
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
                bool gotSymbol;
                const numberOrSymbol = getNumberOrSymbol(gotSymbol);
                if (gotSymbol)
                {
                    // debug writeln("TODO handle floating point: ", numberOrSymbol);
                    _topExprs.insertBack(SExpr(Token(TOK.symbol, numberOrSymbol)));
                }
                else
                {
                    _topExprs.insertBack(SExpr(Token(TOK.number, numberOrSymbol)));
                }
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
                    _topExprs.insertBack(SExpr(Token(TOK.whitespace, null)));
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
                    import nxt.array_algorithm : endsWith;
                    if (symbol.endsWith(`Fn`))
                    {
                        _topExprs.insertBack(SExpr(Token(TOK.functionName, symbol)));
                    }
                    else
                    {
                        _topExprs.insertBack(SExpr(Token(TOK.symbol, symbol)));
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

    public ptrdiff_t offsetTo(scope const char[] expr) const @trusted pure nothrow @nogc
    {
        return expr.ptr - _input.ptr;
    }

    import nxt.line_column : LineColumn, offsetLineColumn;

    public LineColumn offsetToLineColumn(size_t offset) const @trusted pure nothrow @nogc
    {
        return offsetLineColumn(_input, offset);
    }

    public LineColumn sexprToLineColumn(scope const SExpr sexpr) const @trusted pure nothrow @nogc
    {
        return offsetLineColumn(_input, offsetTo(sexpr.token.src));
    }

    public LineColumn charsToLineColumn(scope const(char)[] chars) const @trusted pure nothrow @nogc
    {
        return offsetLineColumn(_input, offsetTo(chars));
    }

private:
    size_t _offset;             // current offset in `_input`
    const Input _input;         // input

    import nxt.fixed_array : FixedArray;
    // import nxt.dynamic_array : DynamicArray;
    alias TopExprs = FixedArray!(SExpr, 1024);
    TopExprs _topExprs;           // top s-expressions (stack)
    size_t _subExprsCount;

    // SExpr[] _subExprsStore;     // sub s-expressions (region)
    // size_t _subExprsOffset = 0; // offset into `_subExprsStore`

    size_t _depth;              // parenthesis depth
    bool _endOfFile;            // signals null terminator found
    bool _includeComments = false;
    bool _includeWhitespace = false;
    bool _disallowEmptyLists = false;
}

/** Parse the contents of `filePath` into lazy range over top-level expressions (`SExpr`).
 *
 * See_Also: https://forum.dlang.org/post/okqdldjnoyrtuizevqeo@forum.dlang.org
 */
struct LispFileParser           // TODO convert to `class`
{
@safe:
    this(const string filePath)
    {
        import std.path : expandTilde;
        import nxt.file_ex : rawReadPath;
        size_t subExprsCount = 0;
        // TODO lookup `subExprsCount` using `filePath` extended attr or hash and pass to constructor
        parser = LispParser(cast(LispParser.Input)filePath.expandTilde.rawReadPath(),
                            false, false, false, subExprsCount);
    }
    ~this() @nogc
    {
        // TODO write parser.subExprsCount
    }
    LispParser parser;
    ptrdiff_t offsetTo(scope const char[] expr) const @safe pure nothrow @nogc
    {
        return parser.offsetTo(expr);
    }
    alias parser this;
}

///
@safe pure unittest
{
    const text = ";;a comment\n(instance AttrFn BinaryFunction);;another comment\0";
    auto _topExprs = LispParser(text);
    assert(!_topExprs.empty);

    assert(_topExprs.front.token.tok == TOK.symbol);
    assert(_topExprs.front.token.src == `instance`);

    assert(_topExprs.front.subs[0].token.tok == TOK.functionName);
    assert(_topExprs.front.subs[0].token.src == "AttrFn");

    assert(_topExprs.front.subs[1].token.tok == TOK.symbol);
    assert(_topExprs.front.subs[1].token.src == "BinaryFunction");

    _topExprs.popFront();
    assert(_topExprs.empty);

}

///
@safe unittest
{
    const text = ";;a comment\n(instance AttrFn BinaryFunction);;another comment\0";
    auto _topExprs = LispParser(text);
    import std.conv : to;
    assert(_topExprs.front.to!string == `(instance AttrFn BinaryFunction)`);
    assert(!_topExprs.empty);
}
